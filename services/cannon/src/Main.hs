module Main (main) where

import Cannon.API
import Cannon.Types
import Data.Yaml (decodeFileEither, ParseException)

decodeConfigFile :: FilePath -> IO (Either ParseException Opts)
decodeConfigFile = decodeFileEither

main :: IO ()
main = do
  configFile <- decodeConfigFile "/etc/wire/cannon.yaml"
  case configFile of
    -- TODO: report config file errors
    Left _ -> parseOptions >>= run
    Right o -> run o
