{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cassandra.Schema
import Control.Exception (finally)
import Data.Monoid ((<>))
import Data.Yaml (decodeFileEither, ParseException)
import Options.Applicative
import System.Logger hiding (info)

import qualified V20
import qualified V21
import qualified V22
import qualified V23

decodeConfigFile :: FilePath -> IO (Either ParseException MigrationOpts)
decodeConfigFile = decodeFileEither

main :: IO ()
main = do
    configFile <- decodeConfigFile "/etc/wire/galley-schema.yaml"
    o <- case configFile of
      Left _ -> execParser (info (helper <*> migrationOptsParser) desc)
      Right opts -> pure opts
    l <- new $ setOutput StdOut . setFormat Nothing $ defSettings
    migrateSchema l o
        [ V20.migration
        , V21.migration
        , V22.migration
        , V23.migration
        ]
      `finally`
        close l
  where
    desc = header "Galley Cassandra Schema" <> fullDesc
