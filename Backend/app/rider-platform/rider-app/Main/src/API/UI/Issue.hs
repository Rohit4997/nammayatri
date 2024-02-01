module API.UI.Issue where

import qualified Domain.Action.Dashboard.Ride as DRide
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import IssueManagement.API.UI.Issue as IA
import IssueManagement.Common.UI.Issue
import qualified IssueManagement.Common.UI.Issue as Common
import qualified IssueManagement.Domain.Action.UI.Issue as Common
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueOption as Domain
import qualified IssueManagement.Domain.Types.Issue.IssueReport as Domain
import Kernel.Beam.Functions
import qualified Kernel.External.Ticket.Interface.Types as TIT
import Kernel.External.Types (Language)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.IssueManagement ()
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import Tools.Auth
import Tools.Error
import Tools.Ticket as TT

type API =
  "issue" :> TokenAuth :> IA.IssueAPI

handler :: FlowServer API
handler = externalHandler
  where
    externalHandler (personId, merchantId) =
      createIssueReport (personId, merchantId)
        :<|> issueReportCustomerList (personId, merchantId)
        :<|> issueMediaUpload (personId, merchantId)
        :<|> fetchMedia (personId, merchantId)
        :<|> getIssueCategory (personId, merchantId)
        :<|> getIssueOption (personId, merchantId)
        :<|> issueInfo (personId, merchantId)
        :<|> updateIssueOption (personId, merchantId)
        :<|> deleteIssue (personId, merchantId)
        :<|> updateIssueStatus (personId, merchantId)

customerIssueHandle :: Common.ServiceHandle Flow
customerIssueHandle =
  Common.ServiceHandle
    { findPersonById = castPersonById,
      findRideById = castRideById,
      findMOCityById = castMOCityById,
      getRideInfo = castRideInfo,
      createTicket = castCreateTicket,
      updateTicket = castUpdateTicket
    }

castPersonById :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Common.Person -> m (Maybe Common.Person)
castPersonById personId = do
  person <- runInReplica $ QP.findById (cast personId)
  return $ fmap castPerson person
  where
    castPerson person =
      Common.Person
        { id = cast person.id,
          language = person.language,
          firstName = person.firstName,
          lastName = person.lastName,
          middleName = person.middleName,
          mobileNumber = person.mobileNumber,
          merchantOperatingCityId = cast person.merchantOperatingCityId
        }

castRideById :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Common.Ride -> Id Common.Merchant -> m (Maybe Common.Ride)
castRideById rideId merchantId = do
  ride <- runInReplica $ QR.findById (cast rideId)
  merchantOpCityId <- case (.merchantOperatingCityId) =<< ride of
    Just moCityId -> return moCityId
    Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (fromMaybe (cast merchantId) ((.merchantId) =<< ride))
  return $ fmap (castRide merchantOpCityId) ride
  where
    castRide moCityId ride = Common.Ride (cast ride.id) (ShortId $ show ride.shortId) (cast moCityId) ride.createdAt

castMOCityById :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Common.MerchantOperatingCity -> m (Maybe Common.MerchantOperatingCity)
castMOCityById moCityId = do
  moCity <- CQMOC.findById (cast moCityId)
  return $ fmap castMOCity moCity
  where
    castMOCity moCity =
      Common.MerchantOperatingCity
        { id = cast moCity.id,
          merchantId = cast moCity.merchantId,
          city = moCity.city
        }

castRideInfo :: (EncFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, EsqDBFlow m r) => Id Common.Merchant -> Id Common.MerchantOperatingCity -> Id Common.Ride -> m Common.RideInfoRes
castRideInfo merchantId _ rideId = do
  rideInfoRes <- DRide.rideInfo (cast merchantId) (cast rideId)
  return $ castRideInfoRes rideInfoRes
  where
    castRideInfoRes res =
      Common.RideInfoRes
        { customerName = res.customerName,
          customerPhoneNo = fromMaybe "" res.customerPhoneNo,
          customerPickupLocation = castLocationAPIEntity res.customerPickupLocation,
          customerDropLocation = castLocationAPIEntity <$> res.customerDropLocation,
          driverName = res.driverName,
          driverPhoneNo = res.driverPhoneNo,
          vehicleNo = res.vehicleNo,
          actualFare = res.actualFare,
          bookingStatus = Nothing
        }

    castLocationAPIEntity ent =
      Common.LocationAPIEntity
        { lat = ent.lat,
          lon = ent.lon,
          street = ent.address.street,
          city = ent.address.city,
          state = ent.address.state,
          country = ent.address.country,
          building = ent.address.building,
          areaCode = ent.address.areaCode,
          area = ent.address.area
        }

castCreateTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.CreateTicketReq -> m TIT.CreateTicketResp
castCreateTicket merchantId merchantOperatingCityId = TT.createTicket (cast merchantId) (cast merchantOperatingCityId)

castUpdateTicket :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r) => Id Common.Merchant -> Id Common.MerchantOperatingCity -> TIT.UpdateTicketReq -> m TIT.UpdateTicketResp
castUpdateTicket merchantId merchantOperatingCityId = TT.updateTicket (cast merchantId) (cast merchantOperatingCityId)

buildMerchantConfig :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id DM.Merchant -> m MerchantConfig
buildMerchantConfig merchantId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  return
    MerchantConfig
      { mediaFileSizeUpperLimit = merchant.mediaFileSizeUpperLimit,
        mediaFileUrlPattern = merchant.mediaFileUrlPattern,
        kaptureDisposition = merchant.kaptureDisposition
      }

issueReportCustomerList :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueReportListRes
issueReportCustomerList (personId, merchantId) language = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Common.issueReportList (cast personId, cast merchantId, cast person.merchantOperatingCityId) language customerIssueHandle CUSTOMER

fetchMedia :: (Id SP.Person, Id DM.Merchant) -> Text -> FlowHandler Text
fetchMedia (personId, merchantId) = withFlowHandlerAPI . Common.fetchMedia (cast personId, cast merchantId)

createIssueReport :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> Common.IssueReportReq -> FlowHandler Common.IssueReportRes
createIssueReport (personId, merchantId) mbLanguage req = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Common.createIssueReport (cast personId, cast merchantId, cast person.merchantOperatingCityId) mbLanguage req (buildMerchantConfig merchantId) customerIssueHandle CUSTOMER

issueMediaUpload :: (Id SP.Person, Id DM.Merchant) -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
issueMediaUpload (personId, merchantId) req = withFlowHandlerAPI $ Common.issueMediaUpload (cast personId, cast merchantId) req (buildMerchantConfig merchantId)

issueInfo :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Maybe Language -> FlowHandler Common.IssueInfoRes
issueInfo (personId, merchantId) issueReportId language = withFlowHandlerAPI $ Common.issueInfo issueReportId (cast personId, cast merchantId) language customerIssueHandle CUSTOMER

updateIssueOption :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Common.IssueUpdateReq -> FlowHandler APISuccess
updateIssueOption (personId, merchantId) issueReportId req = withFlowHandlerAPI $ Common.updateIssueOption issueReportId (cast personId, cast merchantId) req CUSTOMER

deleteIssue :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> FlowHandler APISuccess
deleteIssue (personId, merchantId) issueReportId = withFlowHandlerAPI $ Common.deleteIssue issueReportId (cast personId, cast merchantId) CUSTOMER

getIssueCategory :: (Id SP.Person, Id DM.Merchant) -> Maybe Language -> FlowHandler Common.IssueCategoryListRes
getIssueCategory (personId, merchantId) language = withFlowHandlerAPI $ Common.getIssueCategory (cast personId, cast merchantId) language customerIssueHandle CUSTOMER

getIssueOption :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueCategory -> Maybe (Id Domain.IssueOption) -> Maybe (Id Domain.IssueReport) -> Maybe Language -> FlowHandler Common.IssueOptionListRes
getIssueOption (personId, merchantId) issueCategoryId issueOptionId issueReportId language = withFlowHandlerAPI $ Common.getIssueOption (cast personId, cast merchantId) issueCategoryId issueOptionId issueReportId language customerIssueHandle CUSTOMER

updateIssueStatus :: (Id SP.Person, Id DM.Merchant) -> Id Domain.IssueReport -> Maybe Language -> Common.IssueStatusUpdateReq -> FlowHandler Common.IssueStatusUpdateRes
updateIssueStatus (personId, merchantId) issueReportId language req = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Common.updateIssueStatus (cast personId, cast merchantId, cast person.merchantOperatingCityId) issueReportId language req customerIssueHandle CUSTOMER
