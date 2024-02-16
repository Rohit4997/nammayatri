module DBSync.Delete where

-- import Config.Env
import DBQuery.Functions
import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text as T hiding (elem)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple.Types (Query (..))
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import Types.DBSync
import Types.DBSync.Delete
import Types.Event as Event
import Utils.Utils

-- | This function is used to run the delete operation for a single entry in the stream
runDelete :: (EL.KVDBStreamEntryID, ByteString) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runDelete deleteEntry _ = do
  let (entryId, streamData) = deleteEntry
  EL.logDebug ("BYTE STRING" :: Text) (show streamData)
  case A.eitherDecode @DBDeleteObject . LBS.fromStrict $ streamData of
    Right deleteDBModel -> do
      EL.logDebug ("DB OBJECT" :: Text) (show deleteDBModel)
      runDeleteQuery deleteEntry deleteDBModel
    _ -> do
      EL.logError ("DELETE FAILED" :: Text) ("Invalid streamData or Extraction of data from redis stream failed :: " <> TE.decodeUtf8 streamData)
      return $ Left entryId

runDeleteQuery :: (EL.KVDBStreamEntryID, ByteString) -> DBDeleteObject -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runDeleteQuery deleteEntries dbDeleteObject = do
  Env {..} <- ask
  let (entryId, byteString) = deleteEntries
  let dbModel = dbDeleteObject.dbModel
  let deleteQuery = getDeleteQueryForTable dbDeleteObject
  case deleteQuery of
    Just query -> do
      result <- EL.runIO $ try $ executeQuery _pgConnection (Query $ TE.encodeUtf8 query)
      case result of
        Left (QueryError errorMsg) -> do
          EL.logError ("QUERY DELETE FAILED" :: Text) (errorMsg <> " for query :: " <> query)
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Delete" dbModel.getDBModel
          return $ Left entryId
        Right _ -> do
          EL.logDebug ("QUERY DELETE SUCCESSFUL" :: Text) (" Delete successful for query :: " <> query <> " with streamData :: " <> TE.decodeUtf8 byteString)
          return $ Right entryId
    Nothing -> do
      EL.logError ("No query generated for streamData: " :: Text) (TE.decodeUtf8 byteString)
      return $ Left entryId

getDeleteQueryForTable :: DBDeleteObject -> Maybe Text
getDeleteQueryForTable DBDeleteObject {dbModel, contents, mappings} = do
  let DBDeleteObjectContent whereClause = contents
  let schema = SchemaName $ T.pack currentSchemaName
  generateDeleteQuery DeleteQuery {..}
