module MuchJMAP.App where

import Control.Monad.Catch (Exception, MonadThrow (throwM), MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Set as Set
import MuchJMAP.Config
import MuchJMAP.Fetch
import MuchJMAP.Download
import qualified Data.ByteString.Lazy.Char8 as C
import Network.JMAP.Mail
  ( Email (..),
  )
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>), splitPath, addTrailingPathSeparator, isAbsolute)
import qualified Notmuch
import Control.Monad (forM, forM_, when, mapM_)
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


deleteEmailsExcluding ::
  (MonadIO m, MonadThrow m) =>
  Notmuch.Database Notmuch.RW ->
  FilePath ->
  [Email] ->
  m ()
deleteEmailsExcluding db subdir emails =
  Notmuch.query db (Notmuch.Path subdir)
    >>= handleNotmuchError . Notmuch.messages
    >>= mapM_
      ( \msg -> do
          msg_filename <- Notmuch.messageFilename msg
          when (Set.notMember (emailIdFromFilePath msg_filename) email_ids) $ do
            liftIO $ infoM $ "Deleting email from db: " ++ msg_filename
            handleNotmuchError (Notmuch.removeFile db msg_filename)
            liftIO $ removeFile msg_filename
      )
  where
    email_ids = Set.fromList $ map emailId emails

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
  deleteEmailsExcluding db (configNotmuchDataSubdir config) emails
  forM_ emails $ \email -> do
    let path = dataDir config </> emailFilename email
    liftIO $ infoM $ "Adding email to db: " ++ path
    handleNotmuchError $ Notmuch.indexFile db path
  liftIO $ C.writeFile (stateFilePath config) (Aeson.encode fetch_state)

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
