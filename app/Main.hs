module Main where

import Conduit (runResourceT)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_)
import qualified Data.ByteString.Char8 as C
import Data.Data (Data, Typeable)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import MuchJMAP.App (Config (..))
import qualified MuchJMAP.App as App
import qualified Network.JMAP.API as JMAPAPI
import qualified Network.JMAP.Core as JMAPCore
import qualified Network.JMAP.Mail as JMAPMail
import System.Console.CmdArgs
import System.Log.Logger

data ConfigPath = ConfigPath {configPath :: FilePath}
  deriving (Show, Data, Typeable)

configPathArg = ConfigPath {configPath = def}

main :: IO ()
main = do
  updateGlobalLogger "" (setLevel DEBUG)
  config_path <- cmdArgs configPathArg
  conf <- Yaml.decodeFileThrow $ configPath config_path

  sync_state <- App.runSync conf
  print sync_state

  -- let server_config = configServerConfig conf
  -- let email_filter = configEmailFilter conf

  -- session <- JMAPAPI.getSessionResource server_config
  -- print session
  -- mailboxes <- App.getAllMailbox (server_config, session)
  -- print mailboxes
  -- -- emails <- App.queryEmailIdsFull (server_config, session) (App.encodeEmailFilter mailboxes email_filter)
  -- -- print emails
  -- emails <- App.getAllEmail (server_config, session)
  -- print emails

  -- mvars <-
  --   ( forM emails $ \email -> do
  --       mvar <- newEmptyMVar
  --       forkIO $ do
  --         runResourceT $
  --           JMAPAPI.downloadBlob
  --             (server_config, session)
  --             (JMAPCore.getPrimaryAccount session JMAPCore.MailCapability)
  --             (JMAPMail.emailBlobId email)
  --             "/tmp/mail.txt"
  --         putMVar mvar ()
  --       return mvar
  --     )
  -- forM_ mvars takeMVar
