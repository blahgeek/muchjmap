module MuchJMAP.App where

import Control.Monad.Catch (Exception, MonadThrow, MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import MuchJMAP.Config
import MuchJMAP.Sync
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
  let sync_state_bs = C.toStrict $ Aeson.encode sync_state
  liftIO $ C.writeFile sync_state_filepath (C.fromStrict sync_state_bs)
