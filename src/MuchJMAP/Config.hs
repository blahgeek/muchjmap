module MuchJMAP.Config where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Aeson ((.=))
import Data.Maybe
import System.FilePath ((</>))

import Network.JMAP.API
  ( ServerConfig (..),
  )
import Network.JMAP.Core
  ( FilterCondition (..),
    aesonOptionWithLabelPrefix,
  )
import Network.JMAP.Mail
  ( Mailbox (..),
    MailboxId (..),
    findMailboxByFullName,
  )
import MuchJMAP.Maildir (Maildir, maildirPath)


data EmailFilter = EmailFilter {emailFilterMailboxes :: Maybe [T.Text]}
  deriving (Show, Generic)

instance Aeson.FromJSON EmailFilter where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "emailFilter"


data Config = Config
  { configMaildir :: Maildir,
    configServerConfig :: ServerConfig,
    configEmailFilter :: EmailFilter
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Config where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "config"


pullStateFilePath :: Config -> FilePath
pullStateFilePath conf = maildirPath (configMaildir conf) </> ".muchjmap.pull_state.json"
