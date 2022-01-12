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
import MuchJMAP.App (Config(..))
import qualified MuchJMAP.App as App

data ConfigPath = ConfigPath { configPath :: FilePath }
  deriving (Show, Data, Typeable)

configPathArg = ConfigPath { configPath = def}

main :: IO ()
main = do
  updateGlobalLogger "" (setLevel DEBUG)
  config_path <- cmdArgs configPathArg
  maybe_conf <- Aeson.decodeFileStrict' $ configPath config_path
  let conf = fromJust maybe_conf :: App.Config
  let server_config = configServerConfig conf
  let email_filter = configEmailFilter conf

  session <- JMAPAPI.getSessionResource server_config
  print session
  mailboxes <- App.getAllMailbox (server_config, session)
  print mailboxes
  emails <- App.queryEmailIdsFull (server_config, session) (App.encodeEmailFilter mailboxes email_filter)
  print emails
