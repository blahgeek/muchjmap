module MuchJMAP.App where

import Control.Monad.Catch (Exception, MonadThrow, MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import MuchJMAP.Config
import MuchJMAP.Sync
import MuchJMAP.Download
import qualified Data.ByteString.Lazy.Char8 as C
import System.Directory (doesFileExist)

runApp :: (MonadIO m, MonadThrow m, MonadCatch m) => Config -> m ()
runApp config = do
  let sync_state_filepath = syncStateFilePath config
  sync_state <- liftIO $ do
    file_exists <- doesFileExist sync_state_filepath
    if file_exists then
      Aeson.decodeFileStrict sync_state_filepath
      else return Nothing
  sync_state <- runSync config sync_state

  liftIO $ downloadAllEmails
    (configServerConfig config, syncStateSession sync_state)
    (emailBlobDirectoryPath config)
    (Map.elems (syncStateEmails sync_state))

  let sync_state_bs = C.toStrict $ Aeson.encode sync_state
  liftIO $ C.writeFile sync_state_filepath (C.fromStrict sync_state_bs)
