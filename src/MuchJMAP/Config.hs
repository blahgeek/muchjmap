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

encodeEmailFilter :: [Mailbox] -> EmailFilter -> FilterCondition
encodeEmailFilter mailboxes EmailFilter {emailFilterMailboxes = mailbox_names} =
  FilterOpAND (op_in_mailbox mailbox_names)
  where
    op_in_mailbox Nothing = []
    op_in_mailbox (Just names) =
      let ids = map (mailboxId . fromJust) $ filter isJust $ map (findMailboxByFullName mailboxes) names
       in [FilterOpOR $ map (\m -> FilterValue $ Aeson.object ["inMailbox" .= m]) ids]


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
