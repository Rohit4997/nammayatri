{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverCoin where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Coin as DCoins hiding (CoinStatus)
import Data.OpenApi hiding (title)
import Data.Time (UTCTime (UTCTime, utctDay))
import Domain.Types.Coins.CoinHistory
import Domain.Types.Coins.PurchaseHistory
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Merchant.TransporterConfig ()
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverCoins.Types as DCT
import SharedLogic.DriverFee (delCoinAdjustedInSubscriptionByDriverIdKey, getCoinAdjustedInSubscriptionByDriverIdKey)
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
import Storage.Queries.Coins.CoinHistory as CHistory
import Storage.Queries.Coins.PurchaseHistory as PHistory
import Storage.Queries.DriverPlan as DPlan
import Storage.Queries.DriverStats as QDS
import Storage.Queries.Person as Person
import Tools.Error

data CoinTransactionHistoryItem = CoinTransactionHistoryItem
  { coins :: Int,
    eventFunction :: DCT.DriverCoinsFunctionType,
    bulkUploadTitle :: Maybe DCoins.Translations,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinTransactionRes = CoinTransactionRes
  { coinBalance :: Int,
    coinEarned :: Int,
    coinUsed :: Int,
    coinExpired :: Int,
    todayCoinSummary :: Int,
    coinsEarnedPreviousDay :: Int,
    expiringCoins :: Int,
    expiringDays :: Int,
    coinsExpiredOnThatDay :: Int,
    coinTransactionHistory :: [CoinTransactionHistoryItem]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinUsageHistoryItem = CoinUsageHistoryItem
  { numCoins :: Int,
    createdAt :: UTCTime,
    title :: Text,
    cash :: HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinsUsageRes = CoinsUsageRes
  { coinBalance :: Int,
    totalCoinConvertedToCash :: HighPrecMoney,
    coinConvertedToCashUsedForLatestDues :: Maybe Int,
    coinConvertedTocashLeft :: HighPrecMoney,
    coinConversionRate :: HighPrecMoney,
    coinUsageHistory :: [CoinUsageHistoryItem]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ConvertCoinToCashReq = ConvertCoinToCashReq
  { coins :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type AccumulationResult = [(Text, Int, CoinStatus)]

getCoinEventSummary :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> UTCTime -> Flow CoinTransactionRes
getCoinEventSummary (driverId, merchantId_, merchantOpCityId) dateInUTC = do
  transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId (Just driverId) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  coinBalance_ <- Coins.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  let timeDiffFromUtc = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
  let dateInIst = addUTCTime timeDiffFromUtc dateInUTC
  driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let coinUsed_ = driver.usedCoins
      totalCoinsEarned_ = driver.totalEarnedCoins
      coinExpired_ = totalCoinsEarned_ - (coinBalance_ + coinUsed_)
  coinSummary <- B.runInReplica $ CHistory.getCoinEventSummary driverId dateInIst timeDiffFromUtc
  let todayCoinSummary_ = sum $ map (.coins) coinSummary
  lastDayHistory <- B.runInReplica $ CHistory.getCoinsEarnedLastDay driverId dateInIst timeDiffFromUtc
  let coinsEarnedPreviousDay_ = sum $ map (.coins) lastDayHistory
      coinTransactionHistory = map toTransactionHistoryItem coinSummary
      todayStart = UTCTime (utctDay dateInIst) 0
      coinsExpiredOnThatDay = sumExpiredCoinsOnThatDate coinSummary dateInIst todayStart
  coinsExpiring <- B.runInReplica $ CHistory.getExpiringCoinsInXDay driverId transporterConfig.coinExpireTime timeDiffFromUtc
  let totalExpiringCoins = sum $ map (\coinHistory -> coinHistory.coins - coinHistory.coinsUsed) coinsExpiring
      expiringDays_ = fromIntegral (nominalDiffTimeToSeconds transporterConfig.coinExpireTime) `div` 86400
  pure
    CoinTransactionRes
      { coinBalance = coinBalance_,
        coinEarned = totalCoinsEarned_,
        coinUsed = coinUsed_,
        coinExpired = coinExpired_,
        todayCoinSummary = todayCoinSummary_,
        coinsEarnedPreviousDay = coinsEarnedPreviousDay_,
        coinTransactionHistory = coinTransactionHistory,
        expiringCoins = totalExpiringCoins,
        expiringDays = expiringDays_,
        coinsExpiredOnThatDay = coinsExpiredOnThatDay
      }
  where
    toTransactionHistoryItem :: CoinHistory -> CoinTransactionHistoryItem
    toTransactionHistoryItem historyItem =
      CoinTransactionHistoryItem
        { coins = historyItem.coins,
          eventFunction = historyItem.eventFunction,
          createdAt = historyItem.createdAt,
          bulkUploadTitle = historyItem.bulkUploadTitle
        }

sumExpiredCoinsOnThatDate :: [CoinHistory] -> UTCTime -> UTCTime -> Int
sumExpiredCoinsOnThatDate coinHistories time todayStart = do
  sum $ map (.coins) $ filter isExpirationOnThatDate coinHistories
  where
    isExpirationOnThatDate :: CoinHistory -> Bool
    isExpirationOnThatDate historyItem =
      case expirationAt historyItem of
        Just expiration -> expiration >= todayStart && expiration < time
        Nothing -> False

getCoinUsageSummary :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Integer -> Maybe Integer -> Flow CoinsUsageRes
getCoinUsageSummary (driverId, merchantId_, merchantOpCityId) mbLimit mbOffset = do
  transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId (Just driverId) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  coinBalance_ <- Coins.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  purchaseSummary <- B.runInReplica $ PHistory.getPurchasedHistory driverId mbLimit mbOffset
  mbDriverStat <- QDS.findById driverId
  let coinUsageHistory = map toUsageHistoryItem purchaseSummary
  coinAdjustedInSubscriptionKeyExists <- getCoinAdjustedInSubscriptionByDriverIdKey driverId
  coinConvertedToCashUsage <- case coinAdjustedInSubscriptionKeyExists of
    Just cashUsed -> do
      delCoinAdjustedInSubscriptionByDriverIdKey driverId
      pure $ Just cashUsed
    Nothing -> pure Nothing
  let coinConvertedTocashLeft = mbDriverStat <&> (.coinCovertedToCashLeft)
      totalCoinConvertedToCash = mbDriverStat <&> (.totalCoinsConvertedCash)
  pure
    CoinsUsageRes
      { coinBalance = coinBalance_,
        totalCoinConvertedToCash = fromMaybe 0.0 totalCoinConvertedToCash,
        coinConversionRate = transporterConfig.coinConversionRate,
        coinUsageHistory = coinUsageHistory,
        coinConvertedTocashLeft = fromMaybe 0.0 coinConvertedTocashLeft,
        coinConvertedToCashUsedForLatestDues = coinConvertedToCashUsage
      }
  where
    toUsageHistoryItem :: PurchaseHistory -> CoinUsageHistoryItem
    toUsageHistoryItem historyItem =
      CoinUsageHistoryItem
        { numCoins = historyItem.numCoins,
          createdAt = historyItem.createdAt,
          cash = historyItem.cash,
          title = historyItem.title
        }

accumulateCoins :: () => Int -> [CoinHistory] -> AccumulationResult
accumulateCoins targetAmount = takeCoinsRequired (targetAmount, []) False
  where
    takeCoinsRequired (_, result) True [] = result
    takeCoinsRequired (_, result) False [] = result
    takeCoinsRequired (toTake, result) _ (coinHis : coinHistories) = do
      let availableCoins = coinHis.coins - coinHis.coinsUsed
          coinsTaken = coinHis.coins
          afterTaking = toTake - availableCoins
      if afterTaking > 0
        then do
          takeCoinsRequired (afterTaking, (coinHis.id.getId, coinsTaken, Used) : result) False coinHistories
        else do
          takeCoinsRequired (afterTaking, (coinHis.id.getId, coinsTaken + afterTaking, Remaining) : result) True []

useCoinsHandler :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> ConvertCoinToCashReq -> Flow APISuccess
useCoinsHandler (driverId, merchantId_, merchantOpCityId) ConvertCoinToCashReq {..} = do
  transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId (Just driverId) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless (transporterConfig.coinFeature) $
    throwError $ CoinServiceUnavailable merchantId_.getId
  _ <- DPlan.findByDriverId driverId >>= fromMaybeM (InternalError $ "No plan against the driver id" <> driverId.getId <> "Please choose a plan")
  now <- getCurrentTime
  uuid <- generateGUIDText
  coinBalance <- Coins.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  let stepFunctionToConvertCoins = transporterConfig.stepFunctionToConvertCoins
      timeDiffFromUtc = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
  when (coins < stepFunctionToConvertCoins) $
    throwError $ CoinConversionToCash driverId.getId coins
  when (coins `mod` stepFunctionToConvertCoins /= 0) $
    throwError $ CoinUsedForConverting driverId.getId coins
  let istTime = addUTCTime timeDiffFromUtc now
      currentDate = show $ utctDay istTime
      calculatedAmount = fromIntegral coins * transporterConfig.coinConversionRate
  if coinBalance >= coins
    then do
      let history =
            PurchaseHistory
              { id = Id uuid,
                driverId = driverId.getId,
                merchantId = merchantId_.getId,
                merchantOptCityId = merchantOpCityId.getId,
                numCoins = coins,
                cash = calculatedAmount,
                createdAt = now,
                updatedAt = now,
                title = "converted from coins"
              }
      void $ PHistory.createPurchaseHistory history
      void $ QDS.updateCoinFieldsByDriverId driverId calculatedAmount
      driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      void $ Person.updateUsedCoins driverId (coins + driver.usedCoins)
      histories <- CHistory.getDriverCoinInfo driverId timeDiffFromUtc
      logDebug $ "histories : " <> show histories
      let result = accumulateCoins coins histories
      logDebug $ "result : " <> show result
      mapM_ (\(id, coinValue, status) -> CHistory.updateStatusOfCoins id coinValue status) result
      void $ Hedis.withCrossAppRedis $ Hedis.incrby (Coins.mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral (- coins))
    else do
      throwError $ InsufficientCoins driverId.getId coins
  pure Success
