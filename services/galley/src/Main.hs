module Main (main) where

import Galley.API
import Galley.Options
import OpenSSL

import Data.Yaml (decodeFileEither, ParseException)

decodeConfigFile :: FilePath -> IO (Either ParseException Opts)
decodeConfigFile = decodeFileEither

main :: IO ()
main = withOpenSSL $ do
  configFile <- decodeConfigFile "galley.yaml"
  case configFile of
    -- TODO: report config file errors
    Left _ -> parseOptions >>= run
    Right opts -> run opts
