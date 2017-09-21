{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gundeck.Options
    ( Opts
    , parseOptions
    , hostname
    , serverPort
    , discoUrl

      -- * Storage
    , cassHost
    , cassPort
    , keyspace
    , redisHost
    , redisPort

      -- * AWS
    , queueName
    , awsRegion
    , awsAccount
    , awsArnEnv

      -- * RPC
    , httpPoolSize

      -- * Notifications
    , NotificationTTL (..)
    , notificationTTL

      -- * Fallback Notification Queue
    , fbSkipFallbacks
    , fbQueueLimit
    , fbQueueDelay
    , fbQueueBurst
    ) where

import Cassandra hiding (Error)
import Control.Lens hiding ((.=))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Word
import Data.Maybe (fromMaybe)
import Data.Misc
import Data.Monoid
import Data.Scientific (toBoundedInteger)
import Data.String
import Gundeck.Aws.Arn
import Options.Applicative
import Options.Applicative.Types
import Data.Yaml (FromJSON(..), (.:), (.:?))

import qualified Data.Yaml as Y

newtype NotificationTTL = NotificationTTL
    { notificationTTLSeconds :: Word32 }
    deriving (Eq, Ord, Show)

data Opts = Opts
    { _hostname        :: !String
    , _serverPort      :: !Port
    , _cassHost        :: !String
    , _cassPort        :: !Word16
    , _keyspace        :: !Keyspace
    , _redisHost       :: !String
    , _redisPort       :: !Word16
    , _discoUrl        :: !(Maybe String)
    , _httpPoolSize    :: !Int
    , _queueName       :: !Text
    , _awsRegion       :: !Region
    , _awsAccount      :: !Account
    , _awsArnEnv       :: !ArnEnv
    , _notificationTTL :: !NotificationTTL
    , _fbSkipFallbacks :: !Bool
    , _fbQueueDelay    :: !Word64
    , _fbQueueLimit    :: !Int
    , _fbQueueBurst    :: !Word16
    }

instance FromJSON Opts where
  parseJSON (Y.Object v) =
    Opts <$>
    v .: "host" <*>
    v .: "port" <*>
    v .: "cassandra-host" <*>
    v .: "cassandra-port" <*>
    v .: "cassandra-keyspace" <*>
    v .: "redis-host" <*>
    v .: "redis-port" <*>
    v .:? "disco-url" <*>
    v .: "http-pool-size" <*>
    v .: "queue-name" <*>
    v .: "aws-region" <*>
    v .: "aws-account" <*>
    v .: "aws-arn-env" <*>
    v .: "notification-ttl" <*>
    v .: "fb-skip-fallbacks" <*>
    v .: "fb-queue-delay" <*>
    v .: "fb-queue-limit" <*>
    v .: "fb-queue-burst"

instance FromJSON NotificationTTL where
  parseJSON (Y.Number n) =
    let defaultV = 0
        bounded = toBoundedInteger n :: Maybe Word32
    in pure $ NotificationTTL $ fromMaybe defaultV bounded

instance FromJSON ArnEnv where
  parseJSON (Y.String str) =
    pure $ ArnEnv str

instance FromJSON Account where
  parseJSON (Y.String str) =
    pure $ Account str

instance FromJSON Keyspace where
  parseJSON (Y.String str) =
    pure $ Keyspace str

makeLenses ''Opts

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Gundeck - Push Notifications" <> fullDesc

    optsParser :: Parser Opts
    optsParser = Opts
        <$> (strOption $
                long "host"
                <> value "*4"
                <> showDefault
                <> metavar "HOSTNAME"
                <> help "host to listen on")

        <*> (option auto $
                long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "listen port")

        <*> (strOption $
                long "cassandra-host"
                <> metavar "HOSTNAME"
                <> help "cassandra hostname")

        <*> (option auto $
                long "cassandra-port"
                <> metavar "PORT"
                <> help "cassandra port")

        <*> (fmap Keyspace . textOption $
                long "cassandra-keyspace"
                <> metavar "STRING"
                <> help "database keyspace to use")

        <*> (strOption $
                long "redis-host"
                <> metavar "HOSTNAME"
                <> help "redis hostname")

        <*> (option auto $
                long "redis-port"
                <> metavar "PORT"
                <> help "redis port")

        <*> (optional $ strOption $
                long "disco-url"
                <> metavar "URL"
                <> help "klabautermann url")

        <*> (option auto $
                long "http-pool-size"
                <> metavar "SIZE"
                <> showDefault
                <> help "number of connections for the http client pool"
                <> value 128)

        <*> (textOption $
                long "event-queue-name"
                <> metavar "STRING"
                <> help "sqs queue name")

        <*> (option region $
                long "aws-region"
                <> metavar "STRING"
                <> help "aws region name")

        <*> (fmap Account . textOption $
                long "aws-account"
                <> metavar "STRING"
                <> help "aws account")

        <*> (fmap ArnEnv . textOption $
                long "aws-arn-env"
                <> metavar "STRING"
                <> help "environment name to scope ARNs to")

        <*> (fmap NotificationTTL . option auto $
                long "notification-ttl"
                <> metavar "SIZE"
                <> showDefault
                <> help "TTL (seconds) of stored notifications"
                <> value 86400)

        -- NOTE: If set, notifications are still queued to be sent, etc. but never actually
        --       end up getting sent out. This allows us to still keep track of how successful
        --       we are with cancelling the fallback notifications and thus get a feeling of
        --       where we stand today.
        <*> (switch $
                long "skip-fallbacks"
                <> help "Use this option if you wish to never send delayed fallback notifications.")

        <*> (delayOption $
                long "fallback-queue-delay"
                <> metavar "SIZE"
                <> showDefault
                <> help "Delay (seconds) of notifications before sending a fallback. \
                   \MUST be higher than 30 seconds."
                <> value 300)

        <*> (option auto $
                long "fallback-queue-limit"
                <> metavar "SIZE"
                <> showDefault
                <> help "Max. size of the notification fallback queue."
                <> value 30000)

        <*> (option auto $
                long "fallback-queue-burst"
                <> metavar "SIZE"
                <> showDefault
                <> help "Max. number of delayed notifications to fire in a row (i.e. per second)."
                <> value 100)

    region = readerAsk >>= either readerError return . fromText . fromString

    textOption :: Mod OptionFields String -> Parser Text
    textOption = fmap pack . strOption

    delayOption = fmap check . option auto
      where
        check x = if x < 30 then error "Delay must > 30" else x
