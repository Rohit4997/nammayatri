{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types.DBSync
  ( module X,
    Env (..),
    Types.DBSync.Flow,
    History,
    StateRef (..),
    DBSyncException (..),
    DBCommand (..),
    CreateDBCommand (..),
    UpdateDBCommand (..),
    DeleteDBCommand (..),
    AppCfg (..),
  )
where

import Database.Beam.Postgres (Connection)
import EulerHS.KVConnector.DBSync
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as EL
import EulerHS.Prelude
import qualified EulerHS.Types as ET hiding (Tag)
import Kafka.Producer as Producer
import Kernel.Storage.Esqueleto.Config
import Kernel.Storage.Hedis.Config
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Types.Logging
import Kernel.Utils.Dhall (FromDhall)
import Types.Config
import Types.DBSync.Create as X
import Types.DBSync.Delete as X
import Types.DBSync.Update as X
import Types.Event as Event

data Env = Env
  { _streamRedisInfo :: Text,
    _counterHandles :: Event.DBSyncCounterHandler,
    _kafkaConnection :: Producer.KafkaProducer,
    _pgConnection :: Connection,
    _dontEnableDbTables :: [Text],
    _dontEnableForKafka :: [Text]
  }

data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,
    esqDBReplicaCfg :: EsqDBConfig,
    hedisCfg :: HedisCfg,
    hedisClusterCfg :: HedisCfg,
    kvConfigUpdateFrequency :: Int,
    kafkaProducerCfg :: KafkaProducerCfg,
    maxMessages :: Text,
    loggerConfig :: LoggerConfig,
    dontEnableForDb :: [Text],
    dontEnableForKafka :: [Text]
  }
  deriving (Generic, FromDhall)

type Flow = EL.ReaderFlow Env

type History = [Int]

data StateRef = StateRef
  { _config :: DBSyncConfig,
    _history :: History
  }

data DBSyncException
  = DECODE_ERROR Text
  | CONFIG_NOT_FOUND Text
  | REDIS_STREAM_ERROR ET.KVDBReply
  deriving (Show)

instance Exception DBSyncException

data DBCommand
  = Create DBCommandVersion Tag Double DBName DBCreateObject
  | Update DBCommandVersion Tag Double DBName DBUpdateObject
  | Delete DBCommandVersion Tag Double DBName DBDeleteObject
  deriving (Generic, ToJSON, FromJSON)

data CreateDBCommand = CreateDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName DBCreateObject

data UpdateDBCommand = UpdateDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName DBUpdateObject

data DeleteDBCommand = DeleteDBCommand EL.KVDBStreamEntryID DBCommandVersion Tag Double DBName DBDeleteObject

deriving stock instance Show EL.KVDBStreamEntryID

deriving stock instance Eq EL.KVDBStreamEntryID
