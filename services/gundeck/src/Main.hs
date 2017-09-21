module Main (main) where

import OpenSSL
import Data.Yaml (decodeFileEither, ParseException)

import qualified Gundeck.API as Api
import qualified Gundeck.Options as Options

decodeConfigFile :: FilePath -> IO (Either ParseException Options.Opts)
decodeConfigFile = decodeFileEither

main :: IO ()
main = withOpenSSL $ do
  configFile <- decodeConfigFile "gundeck.yaml"
  case configFile of
    -- TODO: report config file errors
    Left _ -> Options.parseOptions >>= Api.run
    Right opts -> Api.run opts
