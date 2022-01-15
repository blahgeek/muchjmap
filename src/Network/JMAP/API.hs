module Network.JMAP.API
  ( ServerConfig (..),
    getSessionResource,
    apiRequest,
    downloadBlob,
    RequestContext,
  )
where

import Conduit (MonadResource, MonadUnliftIO, sinkFileCautious)
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Functor
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import qualified Network.HTTP.Simple as HTTP
import Network.JMAP.Core
  ( AccountId (..),
    BlobId (..),
    Request (..),
    Response (..),
    SessionResource (sessionApiUrl, sessionDownloadUrl),
    aesonOptionWithLabelPrefix,
  )
import qualified System.Log.Logger as Logger

debugM = Logger.debugM "JMAP.API"

data ServerConfig = ServerConfig
  { serverConfigEndpoint :: T.Text,
    serverConfigUsername :: T.Text,
    serverConfigPassword :: T.Text
  }
  deriving (Show, Generic)

instance Aeson.FromJSON ServerConfig where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "serverConfig"

getSessionResource :: (MonadIO m, MonadThrow m) => ServerConfig -> m SessionResource
getSessionResource config = (http_req >>= HTTP.httpJSON) <&> HTTP.getResponseBody
  where
    http_req =
      HTTP.parseRequest (T.unpack $ serverConfigEndpoint config)
        <&> HTTP.setRequestBasicAuth
          (encodeUtf8 $ serverConfigUsername config)
          (encodeUtf8 $ serverConfigPassword config)
        <&> HTTP.setRequestPath (C.pack "/.well-known/jmap")

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
  where
    http_req =
      HTTP.parseRequest (T.unpack $ sessionApiUrl session)
        <&> HTTP.setRequestBasicAuth
          (encodeUtf8 $ serverConfigUsername config)
          (encodeUtf8 $ serverConfigPassword config)
        <&> HTTP.setRequestMethod (C.pack "POST")
        <&> HTTP.setRequestBodyJSON req

downloadBlob ::
  (MonadUnliftIO m, MonadThrow m, MonadResource m) =>
  RequestContext ->
  AccountId ->
  BlobId ->
  FilePath ->
  m ()
downloadBlob (config, session) (AccountId account_id) (BlobId blob_id) file_path = do
  liftIO $ debugM $ "downloadBlob: " ++ show url ++ " to " ++ file_path
  req <- http_req
  HTTP.httpSink req (\_ -> sinkFileCautious file_path)
  where
    http_req =
      HTTP.parseRequest (T.unpack url)
        <&> HTTP.setRequestBasicAuth
          (encodeUtf8 $ serverConfigUsername config)
          (encodeUtf8 $ serverConfigPassword config)
    url =
      T.replace "{accountId}" account_id
        . T.replace "{blobId}" blob_id
        . T.replace "{type}" "application/octet-stream"
        . T.replace "{name}" "blob"
        $ sessionDownloadUrl session
