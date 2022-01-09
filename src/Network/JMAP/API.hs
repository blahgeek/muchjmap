{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.JMAP.API ( Config(..)
                         , getSessionResource
                         , apiRequest
                         , RequestContext
                         ) where

import Data.ByteString (ByteString)
import Data.Functor
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Control.Monad.Catch

import qualified Network.HTTP.Simple as HTTP
import Network.JMAP.Core ( SessionResource (sessionApiUrl)
                          , Request(..)
                          , Response(..)
                          )

data Config = Config { configEndpoint :: String
                     , configUsername :: ByteString
                     , configPassword :: ByteString}

getSessionResource :: (MonadIO m, MonadThrow m) => Config -> m SessionResource
getSessionResource config = (http_req >>= HTTP.httpJSON) <&> HTTP.getResponseBody
  where http_req =
          HTTP.parseRequest (configEndpoint config) <&>
          HTTP.setRequestBasicAuth (configUsername config) (configPassword config) <&>
          HTTP.setRequestPath (C.pack "/.well-known/jmap")

-- Request API call

type RequestContext = (Config, SessionResource)

apiRequest ::
  (MonadIO m, MonadThrow m) =>
  RequestContext ->
  Request ->
  m Response
apiRequest (config, session) req = (http_req >>= HTTP.httpJSON) <&> HTTP.getResponseBody
  where http_req =
          HTTP.parseRequest (sessionApiUrl session) <&>
          HTTP.setRequestBasicAuth (configUsername config) (configPassword config) <&>
          HTTP.setRequestMethod (C.pack "POST") <&>
          HTTP.setRequestBodyJSON req
