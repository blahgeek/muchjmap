{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.JMAP.API ( Config(..)
                         , getSessionResource
                         , apiRequest
                         , RequestContext
                         ) where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Functor
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Control.Monad.Catch
import GHC.Generics

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Simple as HTTP
import Network.JMAP.Core ( SessionResource (sessionApiUrl)
                          , Request(..)
                          , Response(..)
                          , aesonOptionWithLabelPrefix
                          )

data Config = Config { configEndpoint :: T.Text
                     , configUsername :: T.Text
                     , configPassword :: T.Text}
              deriving (Show, Generic)

instance Aeson.FromJSON Config where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "config"

getSessionResource :: (MonadIO m, MonadThrow m) => Config -> m SessionResource
getSessionResource config = (http_req >>= HTTP.httpJSON) <&> HTTP.getResponseBody
  where http_req =
          HTTP.parseRequest (T.unpack $ configEndpoint config) <&>
          HTTP.setRequestBasicAuth (encodeUtf8 $ configUsername config) (encodeUtf8 $ configPassword config) <&>
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
          HTTP.parseRequest (T.unpack $ sessionApiUrl session) <&>
          HTTP.setRequestBasicAuth (encodeUtf8 $ configUsername config) (encodeUtf8 $ configPassword config) <&>
          HTTP.setRequestMethod (C.pack "POST") <&>
          HTTP.setRequestBodyJSON req
