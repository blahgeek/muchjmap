{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Network.JMAP.API as JMAPAPI
import qualified MuchJMAP.App as App

import System.Console.CmdArgs
import qualified Data.ByteString.Char8 as C
import Data.Data (Data, Typeable)
import Data.Maybe
import qualified Data.Aeson as Aeson

data ConfigPath = ConfigPath { configPath :: FilePath }
  deriving (Show, Data, Typeable)

configPathArg = ConfigPath { configPath = def}

main :: IO ()
main = do
  config_path <- cmdArgs configPathArg
  conf <- Aeson.decodeFileStrict' $ configPath config_path
  session <- JMAPAPI.getSessionResource (fromJust conf)
  print session
  emails <- App.getAllEmail (fromJust conf, session)
  print emails
