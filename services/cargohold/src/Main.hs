module Main (main) where

import CargoHold.API
import CargoHold.Options
import Data.Yaml (decodeFileEither, ParseException)
import OpenSSL

decodeConfigFile :: FilePath -> IO (Either ParseException Opts)
decodeConfigFile = decodeFileEither

main :: IO ()
main = withOpenSSL $ do
  configFile <- decodeConfigFile "cargohold.yaml"
  case configFile of
    -- TODO: report config file errors
    Left _ -> parseOptions >>= start
    Right opts -> start opts
