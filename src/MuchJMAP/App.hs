module MuchJMAP.App where

import Control.Monad.Catch (Exception, MonadThrow, MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import MuchJMAP.Config
import MuchJMAP.Pull
import MuchJMAP.Download
import qualified Data.ByteString.Lazy.Char8 as C
import System.Directory (doesFileExist)

runApp :: (MonadIO m, MonadThrow m, MonadCatch m) => Config -> m ()
runApp config = do
  let pull_state_filepath = pullStateFilePath config
  pull_state <- liftIO $ do
    file_exists <- doesFileExist pull_state_filepath
    if file_exists then
      Aeson.decodeFileStrict pull_state_filepath
      else return Nothing
  pull_state <- runPull config pull_state

  liftIO $ downloadAllEmails
    (configServerConfig config, pullStateSession pull_state)
    (configMaildir config)
    (Map.elems (pullStateEmails pull_state))

  let pull_state_bs = C.toStrict $ Aeson.encode pull_state
  liftIO $ C.writeFile pull_state_filepath (C.fromStrict pull_state_bs)
