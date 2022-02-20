module MuchJMAP.App where

import Control.Monad.Catch (Exception, MonadThrow (throwM), MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import MuchJMAP.Config
import MuchJMAP.Fetch
import MuchJMAP.Download
import qualified Data.ByteString.Lazy.Char8 as C
import Network.JMAP.Mail
  ( Email (..),
  )
import System.Directory (doesFileExist)
import System.FilePath ((</>), splitPath, addTrailingPathSeparator, isAbsolute)
import qualified Notmuch
import Control.Monad (forM, forM_)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified System.Log.Logger as Logger

infoM = Logger.infoM "App"

newtype NotmuchException = NotmuchException Notmuch.Status
  deriving (Show)

instance Exception NotmuchException

handleNotmuchError :: (MonadThrow m) => ExceptT Notmuch.Status m a -> m a
handleNotmuchError block = do
  res <- runExceptT block
  case res of
    Left err -> throwM $ NotmuchException err
    Right ret -> return ret

indexFiles :: (MonadIO m, MonadThrow m) =>
  Config ->
  Notmuch.Database Notmuch.RW ->
  [Email] ->
  m ()
indexFiles config db emails = forM_ emails $ \email -> do
  let path = dataDir config </> emailFilename email
  liftIO $ infoM $ "Adding email to notmuch: " ++ path
  handleNotmuchError $ Notmuch.indexFile db path

runPullFull :: (MonadIO m, MonadThrow m, MonadCatch m) =>
  Config -> m ()
runPullFull config = do
  db <- handleNotmuchError $ Notmuch.databaseOpen (configNotmuchDir config)
  (emails, fetch_state) <- fetchEmailsFull config
  liftIO $
    downloadEmails
      (configServerConfig config, fetchStateSession fetch_state)
      (dataDir config)
      emails
  liftIO $ C.writeFile (stateFilePath config) (Aeson.encode fetch_state)
  indexFiles config db emails

-- runApp :: (MonadIO m, MonadThrow m, MonadCatch m) => Config -> m ()
-- runApp config = do
--   let pull_state_filepath = pullStateFilePath config
--   pull_state <- liftIO $ do
--     file_exists <- doesFileExist pull_state_filepath
--     if file_exists then
--       Aeson.decodeFileStrict pull_state_filepath
--       else return Nothing
--   pull_state <- runPull config pull_state

--   liftIO $ downloadAllEmails
--     (configServerConfig config, pullStateSession pull_state)
--     (configMaildir config)
--     (Map.elems (pullStateEmails pull_state))

--   let pull_state_bs = C.toStrict $ Aeson.encode pull_state
--   liftIO $ C.writeFile pull_state_filepath (C.fromStrict pull_state_bs)
