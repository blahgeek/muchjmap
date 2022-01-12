{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.JMAP.API ( ServerConfig(..)
                         , getSessionResource
                         , apiRequest
                         , RequestContext
                         ) where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Functor
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Control.Monad.IO.Class
import Control.Monad.Catch
import qualified System.Log.Logger as Logger
import GHC.Generics

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Simple as HTTP
import Network.JMAP.Core ( SessionResource (sessionApiUrl)
                          , Request(..)
                          , Response(..)
                          , aesonOptionWithLabelPrefix
                          )

debugM = Logger.debugM "JMAP.API"

data ServerConfig = ServerConfig { serverConfigEndpoint :: T.Text,
                                   serverConfigUsername :: T.Text,
                                   serverConfigPassword :: T.Text }
                  deriving (Show, Generic)

instance Aeson.FromJSON ServerConfig where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "serverConfig"

getSessionResource :: (MonadIO m, MonadThrow m) => ServerConfig -> m SessionResource
getSessionResource config = (http_req >>= HTTP.httpJSON) <&> HTTP.getResponseBody
  where http_req =
          HTTP.parseRequest (T.unpack $ serverConfigEndpoint config) <&>
          HTTP.setRequestBasicAuth
                (encodeUtf8 $ serverConfigUsername config)
                (encodeUtf8 $ serverConfigPassword config) <&>
          HTTP.setRequestPath (C.pack "/.well-known/jmap")

-- Request API call

type RequestContext = (ServerConfig, SessionResource)

apiRequest ::
  (MonadIO m, MonadThrow m) =>
  RequestContext ->
  Request ->
  m Response
apiRequest (config, session) req = do
  liftIO $ debugM $ "apiRequest: " ++ C.unpack (CL.toStrict (Aeson.encode req))
  (http_req >>= HTTP.httpJSON) <&> HTTP.getResponseBody
  where http_req =
          HTTP.parseRequest (T.unpack $ sessionApiUrl session) <&>
          HTTP.setRequestBasicAuth
                (encodeUtf8 $ serverConfigUsername config)
                (encodeUtf8 $ serverConfigPassword config) <&>
          HTTP.setRequestMethod (C.pack "POST") <&>
          HTTP.setRequestBodyJSON req
