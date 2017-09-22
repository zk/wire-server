module Main (main) where

import Data.Yaml (decodeFileEither, ParseException)
import Proxy.API
import Proxy.Options (parseOptions, Opts)

decodeConfigFile :: FilePath -> IO (Either ParseException Opts)
decodeConfigFile = decodeFileEither

main :: IO ()
main = do
  configFile <- decodeConfigFile "proxy.yaml"
  case configFile of
      -- TODO: report config file errors
      Left err -> print err --parseOptions >>= run
      Right opts -> run opts
