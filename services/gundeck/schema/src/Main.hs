{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cassandra.Schema
import Control.Exception (finally)
import Data.Monoid
import Data.Yaml (decodeFileEither, ParseException)
import Options.Applicative
import System.Logger hiding (info)

import qualified V1
import qualified V2
import qualified V3
import qualified V4
import qualified V5
import qualified V6
import qualified V7

decodeConfigFile :: FilePath -> IO (Either ParseException MigrationOpts)
decodeConfigFile = decodeFileEither

main :: IO ()
main = do
    configFile <- decodeConfigFile "/etc/wire/gundeck-schema.yaml"
    o <- case configFile of
        Left _ -> execParser (info (helper <*> migrationOptsParser) desc)
        Right opts -> pure opts
    l <- new $ setOutput StdOut . setFormat Nothing $ defSettings
    migrateSchema l o
        [ V1.migration
        , V2.migration
        , V3.migration
        , V4.migration
        , V5.migration
        , V6.migration
        , V7.migration
        ] `finally` close l
  where
    desc = header "Gundeck Cassandra Schema Migrations" <> fullDesc
