{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module MuchJMAP.JMAP.Request ( Config(..)
                             , getSessionResource
                             , echo
                             ) where

import Data.ByteString (ByteString)
import Data.Functor
import Data.Map (Map)
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Control.Monad.Catch

import qualified Network.HTTP.Simple as HTTP
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import MuchJMAP.JMAP.Types ( SessionResource (sessionApiUrl)
                           , Request(..)
                           , Response
                           , makeEchoMethodCall)

data Config = Config { configEndpoint :: String
                     , configUsername :: ByteString
                     , configPassword :: ByteString}

getSessionResource :: (MonadIO m, MonadThrow m) => Config -> m SessionResource
getSessionResource config = (http_req >>= HTTP.httpJSON) <&> HTTP.getResponseBody
  where http_req =
          HTTP.parseRequest (configEndpoint config) <&>
          HTTP.setRequestBasicAuth (configUsername config) (configPassword config) <&>
          HTTP.setRequestPath (C.pack "/.well-known/jmap")

apiCall ::
  (MonadIO m, MonadThrow m) =>
  Config ->
  SessionResource ->
  Request ->
  m Response
apiCall config session req = (http_req >>= HTTP.httpJSON) <&> HTTP.getResponseBody
  where http_req =
          HTTP.parseRequest (sessionApiUrl session) <&>
          HTTP.setRequestBasicAuth (configUsername config) (configPassword config) <&>
          HTTP.setRequestMethod (C.pack "POST") <&>
          HTTP.setRequestBodyJSON req

echo :: (MonadIO m, MonadThrow m) => Config -> SessionResource -> String -> m String
echo config session msg =
  let method_call = makeEchoMethodCall "id0" (Aeson.object ["message" .= msg]) in do
    response <- apiCall config session (Request [method_call])
    liftIO $ print response
    return ""
