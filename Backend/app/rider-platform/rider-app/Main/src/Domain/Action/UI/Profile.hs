{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Profile
  ( ProfileRes,
    UpdateProfileReq (..),
    UpdateProfileResp,
    UpdateProfileDefaultEmergencyNumbersReq (..),
    PersonDefaultEmergencyNumber (..),
    UpdateProfileDefaultEmergencyNumbersResp,
    GetProfileDefaultEmergencyNumbersResp (..),
    UpdateEmergencySettingsReq (..),
    UpdateEmergencySettingsResp,
    EmergencySettingsRes,
    getPersonDetails,
    updatePerson,
    updateDefaultEmergencyNumbers,
    getDefaultEmergencyNumbers,
    updateEmergencySettings,
    getEmergencySettings,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson as DA
import qualified Data.Aeson.KeyMap as DAKM
import Data.Digest.Pure.MD5 as MD5
import qualified Data.HashMap.Strict as HM
import Data.List (nubBy)
import qualified Data.Text as T
import Domain.Types.Booking.Type as DBooking
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonDefaultEmergencyNumber as DPDEN
import qualified Domain.Types.Person.PersonDisability as PersonDisability
import Environment
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import qualified Kernel.Utils.Text as TU
import Kernel.Utils.Validation
import SharedLogic.Cac
import SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Person as SLP
import SharedLogic.PersonDefaultEmergencyNumber as SPDEN
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Disability as QD
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Person.PersonDefaultEmergencyNumber as QPersonDEN
import qualified Storage.Queries.Person.PersonDisability as PDisability
import Tools.Error

data ProfileRes = ProfileRes
  { id :: Id Person.Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe Text,
    hasTakenRide :: Bool,
    hasTakenValidRide :: Bool,
    referralCode :: Maybe Text,
    whatsappNotificationEnrollStatus :: Maybe Whatsapp.OptApiMethods,
    language :: Maybe Maps.Language,
    hasDisability :: Maybe Bool,
    disability :: Maybe Text,
    gender :: Person.Gender,
    hasCompletedSafetySetup :: Bool,
    hasCompletedMockSafetyDrill :: Maybe Bool,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    followsRide :: Bool,
    frontendConfigHash :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateProfileReq = UpdateProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    deviceToken :: Maybe Text,
    notificationToken :: Maybe Text,
    referralCode :: Maybe Text,
    language :: Maybe Maps.Language,
    gender :: Maybe Person.Gender,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    disability :: Maybe Disability,
    hasDisability :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type UpdateProfileResp = APISuccess.APISuccess

data Disability = Disability
  { id :: Id Disability,
    description :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToSchema, ToJSON, FromJSON)

newtype UpdateProfileDefaultEmergencyNumbersReq = UpdateProfileDefaultEmergencyNumbersReq
  { defaultEmergencyNumbers :: [PersonDefaultEmergencyNumber]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validateUpdateProfileDefaultEmergencyNumbersReq :: Int -> Validate UpdateProfileDefaultEmergencyNumbersReq
validateUpdateProfileDefaultEmergencyNumbersReq maxEmergencyNumberCount UpdateProfileDefaultEmergencyNumbersReq {..} =
  sequenceA_
    [ validateList "defaultEmergencyNumbers" defaultEmergencyNumbers validatePersonDefaultEmergencyNumber,
      validateField "defaultEmergencyNumbers" defaultEmergencyNumbers $ MaxLength maxEmergencyNumberCount
    ]

data PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumber
  { name :: Text,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    priority :: Maybe Int,
    enableForFollowing :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validatePersonDefaultEmergencyNumber :: Validate PersonDefaultEmergencyNumber
validatePersonDefaultEmergencyNumber PersonDefaultEmergencyNumber {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

type UpdateProfileDefaultEmergencyNumbersResp = APISuccess.APISuccess

newtype GetProfileDefaultEmergencyNumbersResp = GetProfileDefaultEmergencyNumbersResp
  { defaultEmergencyNumbers :: [DPDEN.PersonDefaultEmergencyNumberAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getPersonDetails :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> Maybe Int -> m ProfileRes
getPersonDetails (personId, _) mbToss = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  tag <- case person.hasDisability of
    Just True -> B.runInReplica $ fmap (.tag) <$> PDisability.findByPersonId personId
    _ -> return Nothing
  decPerson <- decrypt person
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (\sc -> sc.useCACForFrontend) systemConfigs
  frntndfgs <- if useCACConfig then getFrontendConfigs person mbToss else return $ Just DAKM.empty
  let mbMd5Digest = (T.pack . show . MD5.md5 . DA.encode) <$> frntndfgs
  return $ makeProfileRes decPerson tag mbMd5Digest
  where
    makeProfileRes Person.Person {..} disability md5DigestHash =
      ProfileRes
        { maskedMobileNumber = maskText <$> mobileNumber,
          maskedDeviceToken = maskText <$> deviceToken,
          hasTakenRide = hasTakenValidRide,
          frontendConfigHash = md5DigestHash,
          ..
        }

updatePerson :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Id Person.Person -> UpdateProfileReq -> m APISuccess.APISuccess
updatePerson personId req = do
  mPerson <- join <$> QPerson.findByEmail `mapM` req.email
  whenJust mPerson (\_ -> throwError PersonEmailExists)
  mbEncEmail <- encrypt `mapM` req.email

  refCode <- join <$> validateRefferalCode personId `mapM` req.referralCode

  void $
    QPerson.updatePersonalInfo
      personId
      req.firstName
      req.middleName
      req.lastName
      refCode
      mbEncEmail
      req.deviceToken
      req.notificationToken
      req.language
      req.gender
      req.clientVersion
      req.bundleVersion
  updateDisability req.hasDisability req.disability personId

updateDisability :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Maybe Bool -> Maybe Disability -> Id Person.Person -> m APISuccess.APISuccess
updateDisability hasDisability mbDisability personId = do
  case (hasDisability, mbDisability) of
    (Nothing, _) -> logDebug "No Disability"
    (Just False, _) -> do
      QPerson.updateHasDisability personId $ Just False
      PDisability.deleteByPersonId personId
    (Just True, Nothing) -> throwError $ InvalidRequest "Field disability can't be null if hasDisability is True"
    (Just True, Just selectedDisability) -> do
      customerDisability <- B.runInReplica $ PDisability.findByPersonId personId
      QPerson.updateHasDisability personId $ Just True
      let disabilityId = getId $ selectedDisability.id
      disability <- runInReplica $ QD.findByDisabilityId disabilityId >>= fromMaybeM (DisabilityDoesNotExist disabilityId)
      let mbDescription = (selectedDisability.description) <|> (Just disability.description)
      when (isNothing customerDisability) $ do
        newDisability <- makeDisability selectedDisability disability.tag mbDescription
        PDisability.create newDisability
      when (isJust customerDisability) $ do
        PDisability.updateDisabilityByPersonId personId disabilityId disability.tag mbDescription
      where
        makeDisability personDisability tag mbDescription = do
          now <- getCurrentTime
          return $
            PersonDisability.PersonDisability
              { personId = personId,
                disabilityId = getId $ personDisability.id,
                tag = tag,
                description = mbDescription,
                updatedAt = now
              }
  pure APISuccess.Success

validateRefferalCode :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Id Person.Person -> Text -> m (Maybe Text)
validateRefferalCode personId refCode = do
  unless (TU.validateAllDigitWithMinLength 6 refCode) (throwError $ InvalidRequest "Referral Code must have 6 digits")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId) >>= decrypt
  when (person.hasTakenValidRide) do
    throwError (InvalidRequest "You have been already referred by someone")
  case person.referralCode of
    Just code ->
      if code /= refCode
        then throwError (InvalidRequest "Referral Code is not same")
        else return Nothing -- idempotent behaviour
    Nothing -> do
      merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
      case (person.mobileNumber, person.mobileCountryCode) of
        (Just mobileNumber, Just countryCode) -> do
          void $ CallBPPInternal.linkReferee merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId refCode mobileNumber countryCode
          return $ Just refCode
        _ -> throwError (PersonMobileNumberIsNULL person.id.getId)

updateDefaultEmergencyNumbers ::
  Id Person.Person ->
  Id Merchant.Merchant ->
  UpdateProfileDefaultEmergencyNumbersReq ->
  Flow UpdateProfileDefaultEmergencyNumbersResp
updateDefaultEmergencyNumbers personId merchantId req = do
  maxEmergencyNumberCount <- asks (.maxEmergencyNumberCount)
  oldPersonDENList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  runRequestValidation (validateUpdateProfileDefaultEmergencyNumbersReq maxEmergencyNumberCount) req
  now <- getCurrentTime
  let uniqueRecords = getUniquePersonByMobileNumber req
  newPersonDENList <- buildPersonDefaultEmergencyNumber now `mapM` uniqueRecords
  fork "Send Emergency Contact Added Message" $ do
    sendEmergencyContactAddedMessage personId newPersonDENList oldPersonDENList
  QPersonDEN.replaceAll personId newPersonDENList
  let enableTripShare = any (.enableForFollowing) newPersonDENList
  void $ updateEmergencySettings personId $ updateSettingReq enableTripShare $ not $ null newPersonDENList
  pure APISuccess.Success
  where
    buildPersonDefaultEmergencyNumber now defEmNum = do
      encMobNum <- encrypt defEmNum.mobileNumber
      dbHash <- getDbHash defEmNum.mobileNumber
      mbEmNumPerson <- QPerson.findByMobileNumberAndMerchantId defEmNum.mobileCountryCode dbHash merchantId
      return $
        DPDEN.PersonDefaultEmergencyNumber
          { mobileNumber = encMobNum,
            name = defEmNum.name,
            mobileCountryCode = defEmNum.mobileCountryCode,
            createdAt = now,
            contactPersonId = (.id) <$> mbEmNumPerson,
            enableForFollowing = fromMaybe False defEmNum.enableForFollowing,
            enableForShareRide = False,
            priority = fromMaybe 1 defEmNum.priority,
            merchantId = Just merchantId,
            ..
          }
    updateSettingReq enableTripShare enableEmergencySharing =
      UpdateEmergencySettingsReq
        { shareEmergencyContacts = Just enableEmergencySharing,
          shareTripWithEmergencyContacts = Nothing,
          hasCompletedSafetySetup = Nothing,
          nightSafetyChecks = Nothing,
          shareTripWithEmergencyContactOption = if enableTripShare then Just Person.ALWAYS_SHARE else Nothing
        }

sendEmergencyContactAddedMessage :: Id Person.Person -> [DPDEN.PersonDefaultEmergencyNumber] -> [DPDEN.PersonDefaultEmergencyNumber] -> Flow ()
sendEmergencyContactAddedMessage personId newPersonDENList oldPersonDENList = do
  decNew <- decrypt `mapM` newPersonDENList
  decOld <- decrypt `mapM` oldPersonDENList
  let oldList = (.mobileNumber) <$> decOld
      newList = DPDEN.makePersonDefaultEmergencyNumberAPIEntity False <$> decNew
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  message <-
    MessageBuilder.buildAddedAsEmergencyContactMessage person.merchantOperatingCityId $
      MessageBuilder.BuildAddedAsEmergencyContactMessageReq
        { userName = SLP.getName person,
          appUrl = riderConfig.appUrl
        }
  let filterNewContacts personDEN = return $ not $ personDEN.mobileNumber `elem` oldList
  newlyAddedContacts <- filterM filterNewContacts newList
  SPDEN.notifyEmergencyContacts person (notificationMessage person) "Emergency Contact Added" Notification.EMERGENCY_CONTACT_ADDED (Just message) riderConfig.enableEmergencyContactAddedMessage newlyAddedContacts
  where
    notificationMessage person = "You have been added as an emergency contact by " <> SLP.getName person <> "."

getDefaultEmergencyNumbers :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> m GetProfileDefaultEmergencyNumbersResp
getDefaultEmergencyNumbers (personId, _) = do
  personENList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  decPersonENList <- decrypt `mapM` personENList
  emergencyContactsEntity <- mapM makeAPIEntityAndCheckOnRide decPersonENList
  return $ GetProfileDefaultEmergencyNumbersResp emergencyContactsEntity
  where
    makeAPIEntityAndCheckOnRide :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => DPDEN.DecryptedPersonDefaultEmergencyNumber -> m DPDEN.PersonDefaultEmergencyNumberAPIEntity
    makeAPIEntityAndCheckOnRide personEN = do
      onRide <- case (personEN.contactPersonId, personEN.priority == 0) of
        (Just id, True) -> do
          activeBookings <- runInReplica $ QBooking.findByRiderIdAndStatus id DBooking.activeBookingStatus
          return $ not $ null activeBookings
        _ -> return False
      return $ DPDEN.makePersonDefaultEmergencyNumberAPIEntity onRide personEN

getUniquePersonByMobileNumber :: UpdateProfileDefaultEmergencyNumbersReq -> [PersonDefaultEmergencyNumber]
getUniquePersonByMobileNumber req =
  nubBy ((==) `on` mobileNumber) req.defaultEmergencyNumbers

data UpdateEmergencySettingsReq = UpdateEmergencySettingsReq
  { shareEmergencyContacts :: Maybe Bool,
    shareTripWithEmergencyContacts :: Maybe Bool,
    shareTripWithEmergencyContactOption :: Maybe Person.RideShareOptions,
    nightSafetyChecks :: Maybe Bool,
    hasCompletedSafetySetup :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type UpdateEmergencySettingsResp = APISuccess.APISuccess

updateEmergencySettings :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> UpdateEmergencySettingsReq -> m UpdateEmergencySettingsResp
updateEmergencySettings personId req = do
  personENList <- QPersonDEN.findAllByPersonId personId
  let safetySetupCompleted = guard (req.hasCompletedSafetySetup == Just True) >> Just True
      shareTripOptions = case req.shareTripWithEmergencyContactOption of
        Nothing ->
          case req.shareTripWithEmergencyContacts of
            Just True -> Just Person.SHARE_WITH_TIME_CONSTRAINTS
            Just False -> Just Person.NEVER_SHARE
            Nothing -> Just Person.ALWAYS_SHARE
        _ -> req.shareTripWithEmergencyContactOption
  when (fromMaybe False req.shareEmergencyContacts && null personENList) do
    throwError (InvalidRequest "Add atleast one emergency contact.")
  void $
    QPerson.updateEmergencyInfo
      personId
      req.shareEmergencyContacts
      shareTripOptions
      req.nightSafetyChecks
      safetySetupCompleted
  pure APISuccess.Success

data EmergencySettingsRes = EmergencySettingsRes
  { shareEmergencyContacts :: Bool,
    shareTripWithEmergencyContacts :: Bool,
    shareTripWithEmergencyContactOption :: Person.RideShareOptions,
    hasCompletedMockSafetyDrill :: Bool,
    nightSafetyChecks :: Bool,
    hasCompletedSafetySetup :: Bool,
    defaultEmergencyNumbers :: [DPDEN.PersonDefaultEmergencyNumberAPIEntity],
    enablePoliceSupport :: Bool,
    localPoliceNumber :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getEmergencySettings :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> m EmergencySettingsRes
getEmergencySettings personId = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  personENList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  decPersonENList <- decrypt `mapM` personENList
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  return $
    EmergencySettingsRes
      { shareEmergencyContacts = person.shareEmergencyContacts,
        shareTripWithEmergencyContacts = isJust person.shareTripWithEmergencyContactOption && person.shareTripWithEmergencyContactOption /= Just Person.NEVER_SHARE,
        shareTripWithEmergencyContactOption = fromMaybe Person.NEVER_SHARE person.shareTripWithEmergencyContactOption,
        nightSafetyChecks = person.nightSafetyChecks,
        hasCompletedSafetySetup = person.hasCompletedSafetySetup,
        defaultEmergencyNumbers = DPDEN.makePersonDefaultEmergencyNumberAPIEntity False <$> decPersonENList,
        enablePoliceSupport = riderConfig.enableLocalPoliceSupport,
        localPoliceNumber = riderConfig.localPoliceNumber,
        hasCompletedMockSafetyDrill = fromMaybe False person.hasCompletedMockSafetyDrill
      }
