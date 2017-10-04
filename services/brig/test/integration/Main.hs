{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Bilge (newManager, host, port, Request)
import Cassandra as Cql
import Cassandra.Settings as Cql
import Data.Aeson
import Data.ByteString.Char8 (pack)
import Data.Word
import Data.Yaml (decodeFileEither, ParseException)
import GHC.Generics
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenSSL (withOpenSSL)
import System.Logger (Logger)
import Test.Tasty

import qualified API           as User
import qualified API.Provider  as Provider
import qualified API.Search    as Search
import qualified API.Team      as Team
import qualified API.TURN      as TURN
import qualified API.User.Auth as UserAuth
import qualified System.Logger as Logger

-- TODO: move to common lib
data Endpoint = Endpoint
  { epHost :: String
  , epPort :: Word16
  } deriving (Show, Generic)

data Config = Config
  -- internal endpoints
  { confBrig      :: Endpoint
  , confCannon    :: Endpoint
  , confGalley    :: Endpoint
  , confTurnFile  :: FilePath

  -- databases
  , confCassandra :: Endpoint

  -- test specific settings
  , auth     :: UserAuth.Config
  , provider :: Provider.Config
  , user     :: User.Config
  } deriving (Show, Generic)

instance FromJSON Endpoint
instance FromJSON Config

decodeConfigFile :: FilePath -> IO (Either ParseException Config)
decodeConfigFile = decodeFileEither

withConfig :: FilePath -> (Config -> IO ()) -> IO ()
withConfig path run = do
  config <- decodeConfigFile path
  case config of
    Left err -> print err
    Right conf -> run conf

runTests :: Config -> IO ()
runTests config = do
    let brig      = mkRequest $ confBrig config
        cannon    = mkRequest $ confCannon config
        galley    = mkRequest $ confGalley config
        turnFile  = confTurnFile config
        cassandra = confCassandra config

    lg <- Logger.new Logger.defSettings
    db <- initCassandra cassandra lg
    mg <- newManager tlsManagerSettings

    userApi     <- User.tests (user config) mg brig cannon galley
    userAuthApi <- UserAuth.tests (auth config) mg lg brig
    providerApi <- Provider.tests (provider config) mg db brig cannon galley
    searchApis  <- Search.tests mg brig
    teamApis    <- Team.tests mg brig cannon galley
    turnApi     <- TURN.tests mg brig turnFile

    defaultMain $ testGroup "Brig API Integration"
        [ userApi
        , userAuthApi
        , providerApi
        , searchApis
        , teamApis
        , turnApi
        ]

main :: IO ()
main = withOpenSSL . withConfig "/etc/wire/integration.yaml" $ runTests

initCassandra :: Endpoint -> Logger -> IO Cql.ClientState
initCassandra ep lg =
    Cql.init lg $ Cql.setPortNumber (fromIntegral $ epPort ep)
                . Cql.setContacts (epHost ep) []
                . Cql.setKeyspace (Cql.Keyspace "brig_test")
                $ Cql.defSettings

mkRequest :: Endpoint -> Request -> Request
mkRequest (Endpoint h p) = host (pack h) . port p
