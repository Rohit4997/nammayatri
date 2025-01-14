{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IdfyVerification where

import qualified Domain.Types.DriverOnboarding.Image
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data IdfyVerificationE e = IdfyVerification
  { dashboardPassedVehicleVariant :: Kernel.Prelude.Maybe Domain.Types.Vehicle.Variant,
    docType :: Domain.Types.DriverOnboarding.Image.ImageType,
    documentImageId1 :: Kernel.Types.Id.Id Domain.Types.DriverOnboarding.Image.Image,
    documentImageId2 :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.DriverOnboarding.Image.Image),
    documentNumber :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    driverDateOfBirth :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.IdfyVerification.IdfyVerification,
    idfyResponse :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageExtractionValidation :: Domain.Types.IdfyVerification.ImageExtractionValidation,
    issueDateOnDoc :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    multipleRC :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    nameOnCard :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    requestId :: Kernel.Prelude.Text,
    retryCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    status :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type IdfyVerification = IdfyVerificationE 'AsEncrypted

type DecryptedIdfyVerification = IdfyVerificationE 'AsUnencrypted

instance EncryptedItem IdfyVerification where
  type Unencrypted IdfyVerification = (DecryptedIdfyVerification, HashSalt)
  encryptItem (IdfyVerification {..}, salt) = do
    documentNumber_ <- encryptItem $ (documentNumber, salt)
    return IdfyVerification {documentNumber = documentNumber_, ..}

  decryptItem IdfyVerification {..} = do
    documentNumber_ <- fst <$> decryptItem documentNumber
    return (IdfyVerification {documentNumber = documentNumber_, ..}, "")

instance EncryptedItem' IdfyVerification where
  type UnencryptedItem IdfyVerification = DecryptedIdfyVerification
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data ImageExtractionValidation = Success | Skipped | Failed
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data VerificationStatus = PENDING | VALID | INVALID
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ImageExtractionValidation)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''VerificationStatus)
