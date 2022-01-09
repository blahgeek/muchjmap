{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Network.JMAP.API as JMAPAPI

import System.Log.Logger
import System.Console.CmdArgs
import qualified Data.ByteString.Char8 as C
import Data.Data (Data, Typeable)
import Data.Maybe
import qualified Data.Aeson as Aeson
import qualified MuchJMAP.App as App

data ConfigPath = ConfigPath { configPath :: FilePath }
  deriving (Show, Data, Typeable)

configPathArg = ConfigPath { configPath = def}

main :: IO ()
main = do
  updateGlobalLogger "" (setLevel INFO)
  config_path <- cmdArgs configPathArg
  conf <- Aeson.decodeFileStrict' $ configPath config_path
  session <- JMAPAPI.getSessionResource (fromJust conf)
  print session
  emails <- App.fullQueryEmailIds (fromJust conf, session) Nothing
  print emails
