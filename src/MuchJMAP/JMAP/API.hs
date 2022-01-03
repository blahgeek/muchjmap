{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module MuchJMAP.JMAP.API ( Config(..)
                         , getSessionResource
                         , apiRequest
                         , parseResponseBody
                         , parseResponseBody0
                         , RequestContext
                         ) where

import Data.ByteString (ByteString)
import Data.Functor
import Data.Map (Map)
import qualified Data.ByteString.Char8 as C
import Control.Monad.IO.Class
import Control.Monad.Catch

import qualified Network.HTTP.Simple as HTTP
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import MuchJMAP.JMAP.Core ( SessionResource (sessionApiUrl)
                          , MethodResponse(..)
                          , Request(..)
                          , Response(..)
                          )
import Data.Data (Typeable)

data Config = Config { configEndpoint :: String
                     , configUsername :: ByteString
                     , configPassword :: ByteString}

getSessionResource :: (MonadIO m, MonadThrow m) => Config -> m SessionResource
getSessionResource config = (http_req >>= HTTP.httpJSON) <&> HTTP.getResponseBody
  where http_req =
          HTTP.parseRequest (configEndpoint config) <&>
          HTTP.setRequestBasicAuth (configUsername config) (configPassword config) <&>
          HTTP.setRequestPath (C.pack "/.well-known/jmap")

-- Exceptions
data ParseResponseException = ParseResponseException Response String
  deriving (Show)

instance Exception ParseResponseException

-- Request API call

type RequestContext = (Config, SessionResource)

parseResponseBody ::
  (Aeson.FromJSON a, MonadThrow m) =>
  Int ->     -- idx of response with same id
  String ->  -- id
  Response ->
  m a
parseResponseBody i id response
  | i >= length matched = throwM $
    ParseResponseException response ("Cannot find " ++ show i ++ "-th response of " ++ id)
  | otherwise = case Aeson.Types.parse Aeson.parseJSON (methodResponseBody (matched !! i)) of
      (Aeson.Success val) -> return val
      (Aeson.Error e) -> throwM $ ParseResponseException response e
  where matched = filter (\method_response -> methodResponseId method_response == id) (responseMethodResponses response)

parseResponseBody0 ::
  (Aeson.FromJSON a, MonadThrow m) =>
  String ->  -- id
  Response ->
  m a
parseResponseBody0 = parseResponseBody 0

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
