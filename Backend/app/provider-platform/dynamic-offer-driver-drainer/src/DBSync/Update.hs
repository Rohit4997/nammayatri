{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module DBSync.Update where

import Config.Env
import Control.Exception
import Data.Aeson as A
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Bifunctor as BiFunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Extra (mapLeft)
import Data.Maybe (fromJust)
import qualified Data.Scientific as Sci
import qualified Data.Serialize as Serialize
import Data.Text as T hiding (elem, map)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Database.Beam as B hiding (runUpdate)
import Database.Beam.Postgres
import Database.PostgreSQL.Simple hiding (QueryError)
import Database.PostgreSQL.Simple.Types
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.KVConnector.Types as EKT
import EulerHS.KVConnector.Utils as Utils
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id, try)
import EulerHS.Types as ET
import Kafka.Producer as KafkaProd
import Kafka.Producer as Producer
import qualified Kernel.Beam.Functions as BeamFunction
import Kernel.Beam.Lib.Utils (getMappings, replaceMappings)
import qualified Kernel.Beam.Types as KBT
import Sequelize (Model, Set, Where)
import Text.Casing (pascal)
import Types.DBSync
import Types.Event as Event
import Utils.Utils

updateDB ::
  forall beM be table m.
  ( HasCallStack,
    ET.BeamRuntime be beM,
    ET.BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    EL.MonadFlow m,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Show (table Identity)
  ) =>
  ET.DBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  ByteString ->
  m (Either MeshError ())
updateDB dbConf _ setClause whereClause _ =
  do
    either (pure . Left) (pure . Right) . mapLeft MDBError
    =<< CDB.updateOneWoReturning dbConf Nothing setClause whereClause

getUpdatedValue ::
  forall beM be table m.
  ( HasCallStack,
    ET.BeamRuntime be beM,
    ET.BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    EL.MonadFlow m,
    ToJSON (table Identity),
    FromJSON (table Identity),
    Serialize.Serialize (table Identity)
  ) =>
  Text ->
  Where be table ->
  m (Either MeshError (table Identity))
getUpdatedValue tag _ = do
  res <- EL.runKVDB BeamFunction.meshConfig.kvRedis $ EL.get $ fromString $ T.unpack tag
  case res of
    Right (Just r) -> do
      let (decodeResult :: MeshResult [table Identity], _) = Utils.decodeToField $ BSL.fromChunks [r]
       in case decodeResult of
            Right [decodeRes] -> return $ Right decodeRes
            Right _ -> return $ Left (UnexpectedError "Redis Error: No Data for the key")
            Left _ -> return $ Left (UnexpectedError "Redis Error: Decode Failed")
    _ -> return $ Left (UnexpectedError "Redis Error")

runUpdateCommands :: (UpdateDBCommand, ByteString) -> Text -> Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runUpdateCommands (cmd, val) dbStreamKey = do
  let dbConf = fromJust <$> EL.getOption KBT.PsqlDbCfg
  case cmd of
    UpdateDBCommand id _ tag _ _ (RegistrationTokenOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("RegistrationToken" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (BapMetadataOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("BapMetadata" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (BookingOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Booking" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (BookingCancellationReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("BookingCancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (BusinessEventOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("BusinessEvent" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (CallStatusOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("CallStatus" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (CancellationReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("CancellationReason" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverBlockReasonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverBlockReason" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverPlanOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverPlan" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FleetDriverAssociationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FleetDriverAssociation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverFeeOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverFee" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverInformationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverInformation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (AadhaarOtpReqOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("AadhaarOtpReq" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (AadhaarOtpVerifyOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("AadhaarOtpVerify" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (AadhaarVerificationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("AadhaarVerification" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverLicenseOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverLicense" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverRcAssociationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverRcAssociation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IdfyVerificationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("IdfyVerification" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (ImageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Image" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (VehicleRegistrationCertificateOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("VehicleRegistrationCertificate" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverQuoteOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverQuote" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverReferralOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverReferral" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverStatsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverStats" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (EstimateOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Estimate" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (ExophoneOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Exophone" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FareParametersOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FareParameters" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FareParametersProgressiveDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FareParametersProgressiveDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FareParametersSlabDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FareParametersSlabDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FarePolicyOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FarePolicy" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverExtraFeeBoundsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverExtraFeeBounds" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FarePolicyProgressiveDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FarePolicyProgressiveDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FarePolicyProgressiveDetailsPerExtraKmRateSection" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FarePolicySlabDetailsSlabOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FarePolicySlabDetailsSlab" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RestrictedExtraFareOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("RestrictedExtraFare" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FareProductOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FareProduct" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (GeometryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Geometry" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (CommentOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Comment" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueCategoryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("IssueCategory" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueOptionOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("IssueOption" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueReportOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("IssueReport" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (IssueTranslationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("IssueTranslation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PlaceNameCacheOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("PlaceNameCache" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MediaFileOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MediaFile" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Merchant" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverIntelligentPoolConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverIntelligentPoolConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverPoolConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverPoolConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MandateOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Mandate" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantLeaderBoardConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantLeaderBoardConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantMessageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantMessage" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantPaymentMethodOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantPaymentMethod" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantServiceConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantServiceConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantServiceUsageConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantServiceUsageConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MerchantOnboardingDocumentConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MerchantOnboardingDocumentConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (TransporterConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("TransporterConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MessageOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Message" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MessageReportOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MessageReport" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MessageTranslationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MessageTranslation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (MetaDataOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("MetaData" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PersonOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Person" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (QuoteSpecialZoneOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("QuoteSpecialZone" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RatingOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Rating" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RideOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Ride" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RideDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("RideDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RiderDetailsOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("RiderDetails" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (SearchRequestOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("SearchRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (SearchRequestForDriverOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("SearchRequestForDriver" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (SearchTryOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("SearchTry" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (VehicleOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Vehicle" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (VolunteerOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Volunteer" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FeedbackFormOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FeedbackForm" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FeedbackOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("Feedback" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (FeedbackBadgeOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("FeedbackBadge" :: Text) =<< dbConf
    UpdateDBCommand id _ _ _ _ (BecknRequestOptions _ setClauses whereClause) -> runUpdate id val dbStreamKey setClauses whereClause ("BecknRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (RegistryMapFallbackOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("RegistryMapFallback" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverGoHomeRequestOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverGoHomeRequest" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (DriverHomeLocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("DriverHomeLocation" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (GoHomeConfigOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("GoHomeConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (LocationOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("LocationConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (LocationMappingOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("LocationMappingConfig" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PaymentOrderOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("PaymentOrder" :: Text) =<< dbConf
    UpdateDBCommand id _ tag _ _ (PaymentTransactionOptions _ setClauses whereClause) -> runUpdateInKafkaAndDb id val dbStreamKey setClauses tag whereClause ("PaymentTransaction" :: Text) =<< dbConf
  where
    runUpdate id value _ setClause whereClause model dbConf = do
      maxRetries <- EL.runIO getMaxRetries
      Env {..} <- ask
      if model `elem` _dontEnableDbTables then pure $ Right id else runUpdateWithRetries id value setClause whereClause model dbConf 0 maxRetries
    -- If KAFKA_PUSH is false then entry will be there in DB Else Updates entry in Kafka only.
    runUpdateInKafka id value dbStreamKey' setClause whereClause model dbConf tag = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runUpdate id value dbStreamKey' setClause whereClause model dbConf
        else do
          res <- getUpdatedValue tag whereClause
          case res of
            Right dataObj -> do
              Env {..} <- ask
              let mappings = getMappings [dataObj]
                  newObject = replaceMappings (toJSON dataObj) mappings
              res'' <- EL.runIO $ streamDriverDrainerUpdates _kafkaConnection newObject dbStreamKey' model
              either
                ( \error' -> do
                    void $ publishDBSyncMetric $ Event.KafkaPushFailure "Update" model
                    EL.logError ("ERROR:" :: Text) (("Kafka Driver Update Error " <> error' <> " for model :" <> model) :: Text)
                    pure $ Left (UnexpectedError "Kafka Driver Update Error", id)
                )
                (\_ -> pure $ Right id)
                res''
            Left _ -> do
              let updatedJSON = getDbUpdateDataJson model (A.String "No Data in Redis for updated condition")
              Env {..} <- ask
              res'' <- EL.runIO $ streamDriverDrainerUpdates _kafkaConnection updatedJSON dbStreamKey' model
              either
                ( \error' -> do
                    void $ publishDBSyncMetric $ Event.KafkaPushFailure "Update" model
                    EL.logError ("ERROR:" :: Text) (("Kafka Driver Update Error " <> error' <> " for model :" <> model) :: Text)
                    pure $ Left (UnexpectedError "Kafka Driver Update Error", id)
                )
                (\_ -> pure $ Right id)
                res''

    -- Updates entry in DB if KAFKA_PUSH key is set to false. Else Updates in both.
    runUpdateInKafkaAndDb id value dbStreamKey' setClause tag whereClause model dbConf = do
      isPushToKafka' <- EL.runIO isPushToKafka
      if not isPushToKafka'
        then runUpdate id value dbStreamKey' setClause whereClause model dbConf
        else do
          res <- runUpdateInKafka id value dbStreamKey' setClause whereClause model dbConf tag
          either (\_ -> pure $ Left (UnexpectedError "Kafka Error", id)) (\_ -> runUpdate id value dbStreamKey' setClause whereClause model dbConf) res

    runUpdateWithRetries id value setClause whereClause model dbConf retryIndex maxRetries = do
      res <- updateDB dbConf Nothing setClause whereClause value
      case (res, retryIndex) of
        (Left _, y) | y < maxRetries -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" model
          EL.runIO $ delay =<< getRetryDelay
          runUpdateWithRetries id value setClause whereClause model dbConf (retryIndex + 1) maxRetries
        (Left _, _) -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" model
          EL.logError (("Update failed for model: " <> show model) :: Text) (show [("command" :: String, value)] :: Text)
          pure $ Left (UnexpectedError "Update failed for model", id)
        (Right _, _) -> do
          pure $ Right id

streamDriverDrainerUpdates :: ToJSON a => Producer.KafkaProducer -> a -> Text -> Text -> IO (Either Text ())
streamDriverDrainerUpdates producer dbObject dbStreamKey model = do
  let topicName = "adob-sessionizer-" <> T.toLower model
  result' <- KafkaProd.produceMessage producer (message topicName dbObject)
  case result' of
    Just err -> pure $ Left $ T.pack ("Kafka Error: " <> show err)
    _ -> pure $ Right ()
  where
    message topicName event =
      ProducerRecord
        { prTopic = TopicName topicName,
          prPartition = UnassignedPartition,
          prKey = Just $ TE.encodeUtf8 dbStreamKey,
          prValue = Just . LBS.toStrict $ encode event
        }

getDbUpdateDataJson :: ToJSON a => Text -> a -> A.Value
getDbUpdateDataJson model a =
  A.object
    [ "contents"
        .= A.toJSON a,
      "tag" .= T.pack (pascal (T.unpack model) <> "Object"),
      "type" .= ("UPDATE" :: Text)
    ]

updValToJSON :: [(Text, A.Value)] -> A.Value
updValToJSON keyValuePairs = A.Object $ AKM.fromList . map (BiFunctor.first AesonKey.fromText) $ keyValuePairs

getPKeyandValuesList :: Text -> [(Text, A.Value)]
getPKeyandValuesList pKeyAndValue = go (T.splitOn "_" pKeyTrimmed) []
  where
    go (tName : k : v : rest) acc = go (tName : rest) ((k, A.String v) : acc)
    go _ acc = acc
    pKeyTrimmed = case T.splitOn "{" pKeyAndValue of
      [] -> ""
      (x : _) -> x

newtype QueryError = QueryError Text
  deriving (Show)

instance Exception QueryError

-- Execute a query and throw a custom error if it fails
executeQuery :: Connection -> Query -> IO ()
executeQuery conn query' = do
  result <- try $ execute_ conn query' :: IO (Either SomeException Int64)
  case result of
    Left e -> throwIO $ QueryError $ "Query execution failed: " <> T.pack (show e)
    Right _ -> return ()

runUpdateQuery :: (EL.KVDBStreamEntryID, ByteString) -> Text -> ReaderT Env EL.Flow (Either EL.KVDBStreamEntryID EL.KVDBStreamEntryID)
runUpdateQuery updateDataEntries _ =
  let (mbModel, mbObject, mbMappings) = getData (snd updateDataEntries)
      updateQuery = getUpdateQueryForTable (mbModel, mbObject, mbMappings)
   in case updateQuery of
        Just query' -> do
          Env {..} <- ask
          result <- EL.runIO $ try $ executeQuery _pgConnection (Query $ TE.encodeUtf8 query')
          case result of
            Left (QueryError errorMsg) -> do
              EL.logError ("QUERY UPDATE FAILED" :: Text) (errorMsg <> " for query :: " <> query')
              pure $ Left (fst updateDataEntries)
            Right _ -> do
              EL.logInfo ("QUERY UPDATE SUCCESSFUL" :: Text) (" Update successful for query :: " <> query' <> " with streamData :: " <> TE.decodeUtf8 (snd updateDataEntries))
              pure $ Right (fst updateDataEntries)
        Nothing -> do
          EL.logInfo ("No query generated for streamData: " :: Text) (TE.decodeUtf8 (snd updateDataEntries))
          pure $ Left (fst updateDataEntries)

-- This function is used to extract the model name, object and mappings from the stream data
getData :: ByteString -> (Maybe T.Text, Maybe A.Array, Maybe A.Object)
getData dbCommandByteString = do
  case A.decode $ LBS.fromStrict dbCommandByteString of
    Just _decodedDBCommandObject@(A.Object o) ->
      let mbModel = case HM.lookup "contents" o of
            Just _commandArray@(A.Array a) -> case V.last a of
              _commandObject@(A.Object command) -> case HM.lookup "tag" command of
                Just (A.String modelTag) -> pure $ T.pack (quietSnake (T.unpack (T.take (T.length modelTag - 7) modelTag)))
                _ -> Nothing
              _ -> Nothing
            _ -> Nothing

          mbSetAndWhereArray = case HM.lookup "contents" o of
            Just _commandArray@(A.Array a) -> case V.last a of
              _commandObject@(A.Object command) -> case HM.lookup "contents" command of
                Just (A.Array ar) -> return ar
                _ -> Nothing
              _ -> Nothing
            _ -> Nothing

          mbMapings = case HM.lookup "mappings" o of
            Just (A.Object obj) -> return obj
            _ -> Nothing

          updatedModel = case HM.lookup "updatedModel" o of
            Just (A.Object obj) -> return obj
            _ -> Nothing
       in (mbModel, mbSetAndWhereArray, mbMapings)
    _ -> (Nothing, Nothing, Nothing)

getUpdateQueryForTable :: (Maybe Text, Maybe Array, Maybe Object) -> Maybe Text
getUpdateQueryForTable (mbModel, mbArray, mbMappings) = do
  case (mbModel, mbArray) of
    (Just model, Just array') -> do
      let (setValues, [whereValues]) = getSetAndWhereClause (Just array')
          correctWhereClauseText = makeWhereCondition whereValues (fromMaybe HM.empty mbMappings)
          setQuery = makeSetConditions setValues (fromMaybe HM.empty mbMappings)
      if T.null correctWhereClauseText
        then Nothing
        else Just $ "UPDATE atlas_driver_offer_bpp." <> quote' (textToSnakeCaseText model) <> " SET " <> setQuery <> " WHERE " <> correctWhereClauseText <> ";"
    _ -> Nothing

makeSetConditions :: [(Text, Value)] -> A.Object -> Text
makeSetConditions setValues mbMappings = do
  let setClauseText = map (second valueToText) setValues
  let correctSetClausetext = map (\(k, v) -> (replaceMappings k mbMappings, v)) setClauseText
  T.intercalate "," (map (\(k, v) -> (quote' . textToSnakeCaseText) k <> "=" <> v) correctSetClausetext)

quote' :: Text -> Text
quote' t = "\"" <> t <> "\""

quote :: Text -> Text
quote t = "'" <> t <> "'"

makeWhereCondition :: Value -> A.Object -> Text
makeWhereCondition values mappings = case values of
  A.Object obj -> do
    let key = HM.keys obj
    case key of
      ["$and"] -> getArrayConditionText (fromMaybe A.emptyObject (HM.lookup "$and" obj)) " AND " mappings
      ["$or"] -> getArrayConditionText (fromMaybe A.emptyObject (HM.lookup "$or" obj)) " OR " mappings
      -- these conditions need to be implemented safely before using in production
      -- ["$gt"] -> "( " <>
      -- ["$gte"] -> " >= " <>
      -- ["$lt"] -> " < " <>
      -- ["$lte"] -> " <= " <>
      -- ["$ne"] -> " <> " <>
      -- ["$notIn"] -> " NOT IN " <>
      -- ["$in"] -> " IN " <>
      [key'] -> quote' (textToSnakeCaseText $ replaceMappings key' mappings) <> " = " <> textToSnakeCaseText (valueToText (fromMaybe A.Null (HM.lookup key' obj)))
      _ -> ""
  _ -> ""

getArrayConditionText :: Value -> Text -> A.Object -> Text
getArrayConditionText arr cnd mappings = case arr of
  A.Array arr' -> case V.toList arr' of
    [] -> ""
    [x] -> makeWhereCondition x mappings
    (x : xs) -> "(" <> makeWhereCondition x mappings <> ")" <> cnd <> getArrayConditionText (A.Array (V.fromList xs)) cnd mappings
  _ -> ""

textToSnakeCaseText :: Text -> Text
textToSnakeCaseText = T.pack . quietSnake . T.unpack

replaceMappings :: Text -> A.Object -> Text
replaceMappings element obj =
  case HM.lookup element obj of
    Just (A.String value) -> value
    _ -> element

extractValues :: Value -> [Value]
extractValues (Array arr) = toList arr
extractValues _ = []

extractBothValues :: Value -> Maybe (Text, Value)
extractBothValues (Object obj) = do
  mbVal <- (,) <$> HM.lookup "value0" obj <*> HM.lookup "value1" obj
  case mbVal of
    (A.String val0, val1) -> Just (val0, val1)
    _ -> Nothing
extractBothValues _ = Nothing

getSetAndWhereClause :: Maybe A.Array -> ([(Text, A.Value)], [A.Value])
getSetAndWhereClause jsonArray = case toList <$> jsonArray of
  Just [firstArray, secondArray] -> do
    let firstList = extractValues firstArray
        secondList = extractValues secondArray
        setValues = mapMaybe extractBothValues firstList
        whereValues = mapMaybe extractBothValues secondList
        whereConditions = map snd whereValues
     in (setValues, whereConditions)
  _ -> ([], [])

valueToText :: A.Value -> T.Text
valueToText (A.String t) = quote t
valueToText (A.Number n) =
  quote $
    if Sci.isInteger n
      then T.pack (show (Sci.coefficient n)) -- Convert to integer if it's an integer
      else T.pack (show (Sci.toRealFloat n)) -- Convert to floating-point
valueToText (A.Bool b) = quote $ if b then "true" else "false"
valueToText (A.Array a) = quote $ "{" <> T.intercalate "," (map valueToText' (V.toList a)) <> "}" --in case of array of value of a key in object
valueToText (A.Object obj) = quote $ T.pack (show (A.encode obj))
valueToText A.Null = "null"

valueToText' :: A.Value -> T.Text
valueToText' (A.String t) = t
valueToText' (A.Number n) =
  if Sci.isInteger n
    then T.pack (show (Sci.coefficient n)) -- Convert to integer if it's an integer
    else T.pack (show (Sci.toRealFloat n)) -- Convert to floating-point
valueToText' (A.Bool b) = if b then "true" else "false"
valueToText' (A.Array a) = "{" <> T.intercalate "," (map valueToText' (V.toList a)) <> "}" --in case of array of value of a key in object
valueToText' (A.Object obj) = T.pack (show (A.encode obj))
valueToText' _ = "null"

-- {
--     "tag": "Update",
--     "contents": [
--         [],
--         "customer_id_cth_wVpvhsvczuQZ7Cic{shard-8}",
--         1672405027778,
--         "ECRDB",
--         {
--             "tag": "CustomerOptions",
--             "contents": [
--                 [
--                     {
--                         "value1": "2022-12-30T12:57:07Z",
--                         "value0": "lastUpdated"
--                     },
--                     {
--                         "value1": "7076607678",
--                         "value0": "mobileNumber"
--                     },
--                     {
--                         "value1": "91",
--                         "value0": "mobileCountryCode"
--                     },
--                     {
--                         "value1": "malav42@gmail.com",
--                         "value0": "emailAddress"
--                     },
--                     {
--                         "value1": "Malav12",
--                         "value0": "firstName"
--                     },
--                     {
--                         "value1": "Shawn124",
--                         "value0": "lastName"
--                     }
--                 ],
--                 {
--                     "value1": {
--                         "$and": [
--                             {
--                                 "$or": [
--                                     {
--                                         "id": "cth_wVpvhsvczuQZ7Cic"
--                                     },
--                                     {
--                                         "objectReferenceId": "cth_wVpvhsvczuQZ7Cic"
--                                     }
--                                 ]
--                             },
--                             {
--                                 "merchantAccountId": 80
--                             }
--                         ]
--                     },
--                     "value0": "where"
--                 }
--             ]
--         }
--     ]
-- }
