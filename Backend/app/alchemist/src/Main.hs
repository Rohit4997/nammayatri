{-# LANGUAGE OverloadedStrings #-}

module Main where

--import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
--import qualified Data.Yaml as Yaml
import Kernel.Prelude
import qualified NammaDSL.App as NammaDSL
import System.Directory
import System.Environment (getArgs, setEnv)
import System.FilePath

findGitRoot :: FilePath -> IO (Maybe FilePath)
findGitRoot dir = do
  let gitPath = dir </> ".git"
  exists <- doesDirectoryExist gitPath
  if exists
    then return (Just dir)
    else
      let parent = takeDirectory dir
       in if parent == dir
            then return Nothing -- No more directories to check
            else findGitRoot parent

-- applyDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
-- applyDirectory dirPath processFile = do
--   exists <- doesDirectoryExist dirPath
--   when exists $ do
--     files <- listDirectory dirPath
--     let yamlFiles = filter (\file -> takeExtension file `elem` [".yaml", ".yml"]) files
--     mapM_ (processFile . (dirPath </>)) yamlFiles

-- data Apps = DriverApp | RiderApp deriving (Generic, FromJSON, Show)

-- data LibraryPaths = LibraryPaths
--   { libPath :: FilePath,
--     usedIn :: [Apps]
--   }
--   deriving (Generic, FromJSON, Show)

-- getFilePathsForConfiguredApps :: FilePath -> IO [LibraryPaths]
-- getFilePathsForConfiguredApps rootDir = do
--   contents <- BS.readFile $ rootDir </> "Backend/dslLibs.yaml"
--   case Yaml.decodeEither' contents of
--     Left err -> error $ show err
--     Right yml -> return yml

processSpecFolders :: Bool -> FilePath -> IO ()
processSpecFolders isGenAll specFolderPath = do
  contents <- listDirectory specFolderPath
  forM_ contents $ \entry -> do
    let entryPath = specFolderPath </> entry
    isDirectory <- doesDirectoryExist entryPath
    when isDirectory $ do
      let isSpecDir = "/spec" `isSuffixOf` entryPath
      if isSpecDir
        then do
          let apiFolderPath = entryPath </> "API"
              storageFolderPath = entryPath </> "Storage"
              configPath = entryPath </> "dsl-config.dhall"
          apiContents <- doesDirectoryExist apiFolderPath >>= bool (pure []) (listDirectory apiFolderPath)
          storageContents <- doesDirectoryExist storageFolderPath >>= bool (pure []) (listDirectory storageFolderPath)
          isConfigExists <- doesFileExist configPath
          if not isConfigExists
            then do
              putStrLn' "33" ("Skipping as Config file not found at " ++ configPath)
            else do
              forM_ apiContents $
                \inputFile -> do
                  let inputFilePath = apiFolderPath </> inputFile
                  fileState <- NammaDSL.getFileState inputFilePath
                  putStrLn $ show fileState ++ " " ++ inputFile
                  when (isGenAll || fileState == NammaDSL.NEW || fileState == NammaDSL.CHANGED) $
                    NammaDSL.runApiGenerator configPath inputFilePath

              forM_ storageContents $
                \inputFile -> do
                  let inputFilePath = storageFolderPath </> inputFile
                  fileState <- NammaDSL.getFileState inputFilePath
                  putStrLn $ show fileState ++ " " ++ inputFile
                  when (isGenAll || fileState == NammaDSL.NEW || fileState == NammaDSL.CHANGED) $
                    NammaDSL.runStorageGenerator configPath inputFilePath
        else processSpecFolders isGenAll entryPath

putStrLn' :: String -> String -> IO ()
putStrLn' colorCode text = putStrLn $ "\x1b[" ++ colorCode ++ "m" ++ text ++ "\x1b[0m"

main :: IO ()
main = do
  putStrLn ("Version " ++ NammaDSL.version)
  generateAllSpecs <- ("--all" `elem`) <$> getArgs
  currentDir <- getCurrentDirectory
  maybeGitRoot <- findGitRoot currentDir
  let rootDir = fromMaybe (error "Could not find git root") maybeGitRoot
  setEnv "GIT_ROOT_PATH" (show rootDir)
  putStrLn' "32" ("Root dir: " ++ rootDir)
  processSpecFolders generateAllSpecs rootDir

-- What about Library apps ? --TODO
-- mapM_
--   ( \libPaths ->
--       mapM_
--         ( \lib -> do
--             let (databaseName, appName) =
--                   case lib of
--                     DriverApp -> (driverAppDatabaseName, driverAppName)
--                     RiderApp -> (riderAppDatabaseName, riderAppDatabaseName)
--             processApp generateAllSpecs databaseName rootDir libPaths.libPath appName
--         )
--         libPaths.usedIn
