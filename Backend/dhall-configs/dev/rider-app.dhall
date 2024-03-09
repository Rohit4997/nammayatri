let common = ./common.dhall

let sec = ./secrets/rider-app.dhall

let globalCommon = ../generic/common.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5436
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_app"
      , connectionPoolCount = +25
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = 5436
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      , connectionPoolCount = esqDBCfg.connectionPoolCount
      }

let rcfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let hcfg =
      { connectHost = rcfg.connectHost
      , connectPort = rcfg.connectPort
      , connectAuth = rcfg.connectAuth
      , connectDatabase = rcfg.connectDatabase
      , connectMaxConnections = rcfg.connectMaxConnections
      , connectMaxIdleTime = rcfg.connectMaxIdleTime
      , connectTimeout = rcfg.connectTimeout
      }

let smsConfig =
      { sessionConfig = common.smsSessionConfig
      , credConfig =
        { username = common.smsUserName
        , password = common.smsPassword
        , otpHash = sec.smsOtpHash
        }
      , useFakeSms = Some 7891
      , url = "http://localhost:4343"
      , sender = "JUSPAY"
      }

let InfoBIPConfig =
      { username = common.InfoBIPConfig.username
      , password = common.InfoBIPConfig.password
      , token = common.InfoBIPConfig.token
      , url = "https://gye1yw.api.infobip.com"
      , webhookurl = "http://localhost:8013/v2/update/status"
      , sender = "JUSPAY"
      }

let sampleKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "rider-app-events-updates", kafkaKey = "rider-app" }

let autoCompleteKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "AutoCompleteData"
      , kafkaKey = "rider-app-autocomplete-events"
      }

let routeDataKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "RouteCollection"
      , kafkaKey = "rider-app-route-data-events"
      }

let exophoneKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "ExophoneData", kafkaKey = "rider-app-exophone-events" }

let sampleLogConfig
    : Text
    = "log-stream"

let samplePrometheusConfig
    : Text
    = "prometheus-stream"

let eventStreamMappings =
      [ { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig = globalCommon.streamConfig.KafkaStream sampleKafkaConfig
        , eventTypes =
          [ globalCommon.eventType.RideCreated
          , globalCommon.eventType.RideStarted
          , globalCommon.eventType.RideEnded
          , globalCommon.eventType.RideCancelled
          , globalCommon.eventType.BookingCreated
          , globalCommon.eventType.BookingCancelled
          , globalCommon.eventType.BookingCompleted
          , globalCommon.eventType.SearchRequest
          , globalCommon.eventType.Quotes
          , globalCommon.eventType.Estimate
          ]
        }
      , { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig =
            globalCommon.streamConfig.KafkaStream exophoneKafkaConfig
        , eventTypes = [ globalCommon.eventType.ExophoneData ]
        }
      , { streamName = globalCommon.eventStreamNameType.LOG_STREAM
        , streamConfig = globalCommon.streamConfig.LogStream sampleLogConfig
        , eventTypes =
          [ globalCommon.eventType.RideEnded
          , globalCommon.eventType.RideCancelled
          ]
        }
      , { streamName = globalCommon.eventStreamNameType.PROMETHEUS_STREAM
        , streamConfig =
            globalCommon.streamConfig.PrometheusStream samplePrometheusConfig
        , eventTypes =
          [ globalCommon.eventType.RideCreated
          , globalCommon.eventType.SearchRequest
          ]
        }
      , { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig =
            globalCommon.streamConfig.KafkaStream autoCompleteKafkaConfig
        , eventTypes = [ globalCommon.eventType.AutoCompleteData ]
        }
      , { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig =
            globalCommon.streamConfig.KafkaStream routeDataKafkaConfig
        , eventTypes = [ globalCommon.eventType.RouteCollection ]
        }
      ]

let apiRateLimitOptions = { limit = +8000, limitResetTimeInSec = +1 }

let searchRateLimitOptions = { limit = +8000, limitResetTimeInSec = +1 }

let slackCfg =
      { channelName = "#beckn-driver-onboard-test"
      , slackToken = common.slackToken
      }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let rideConfig =
      { driverReachedDistance = +100, driverOnTheWayNotifyExpiry = +3600 }

let cacheConfig = { configsExpTime = +86400 }

let cacheTranslationConfig = { expTranslationTime = +3600 }

let cacheFeedbackFormConfig = { configsExpTime = +5184000 }

let hccfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let kvConfigUpdateFrequency = +10

let RiderJobType = < CheckPNAndSendSMS | OtherJobTypes >

let jobInfoMapx =
      [ { mapKey = RiderJobType.CheckPNAndSendSMS, mapValue = True }
      , { mapKey = RiderJobType.OtherJobTypes, mapValue = False }
      ]

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = hcfg
    , hedisClusterCfg = hccfg
    , hedisNonCriticalCfg = hcfg
    , hedisNonCriticalClusterCfg = hccfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = True
    , cutOffNonCriticalHedisCluster = False
    , smsCfg = smsConfig
    , infoBIPCfg = InfoBIPConfig
    , port = +8013
    , metricsPort = +9999
    , hostName = "localhost"
    , nwAddress = "http://localhost:8013/beckn/cab/v1"
    , selfUIUrl = "http://localhost:8013/v2/"
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , s3Config = common.s3Config
    , s3PublicConfig = common.s3PublicConfig
    , searchRequestExpiry = Some +600
    , migrationPath =
      [ "dev/migrations-read-only/rider-app"
      , env:RIDER_APP_MIGRATION_PATH as Text ? "dev/migrations/rider-app"
      ]
    , autoMigrate = True
    , coreVersion = "0.9.4"
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/rider-app.log", logRawSql = True }
    , googleTranslateUrl = common.googleTranslateUrl
    , googleTranslateKey = common.googleTranslateKey
    , internalAPIKey = sec.internalAPIKey
    , metricsSearchDurationTimeout = +45
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , searchRateLimitOptions
    , slackCfg
    , searchLimitExceedNotificationTemplate =
        "Customer with {#cust-id#} is exceeding the search limit."
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , authTokenCacheExpiry = +600
    , disableSignatureAuth = False
    , encTools
    , kafkaProducerCfg
    , rideCfg = rideConfig
    , dashboardToken = sec.dashboardToken
    , cacheConfig
    , cacheTranslationConfig
    , cacheFeedbackFormConfig
    , maxEmergencyNumberCount = +3
    , minTripDistanceForReferralCfg = Some +1000
    , enableRedisLatencyLogging = False
    , enablePrometheusMetricLogging = True
    , eventStreamMap = eventStreamMappings
    , kvConfigUpdateFrequency
    , incomingAPIResponseTimeout = +15
    , maxShards = +5
    , jobInfoMapx
    , internalEndPointMap = common.internalEndPointMap
    , schedulerSetName = "rider-scheduler-set"
    , schedulerType = common.schedulerType.RedisBased
    , isBecknSpecVersion2 = True
    , _version = "2.0.0"
    , hotSpotExpiry = +604800
    , collectRouteData = True
    }
