module Network.JMAP.Mail where

import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Data (Data)
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import GHC.Generics
import Network.JMAP.Core
  ( BlobId (BlobId),
    Capability (..),
    CommonGetResponseBody (..),
    MethodCall (..),
    MethodCallArg (..),
    MethodCallArgs (..),
    Request (..),
    Response,
    aesonOptionWithLabelPrefix,
    fieldLabels,
    getPrimaryAccount,
    methodCallArgFrom,
    methodCallArgsFrom,
  )

newtype MailboxId = MailboxId T.Text
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show, Aeson.FromJSONKey, Aeson.ToJSONKey, Eq, Data)

instance Ord MailboxId where
  compare (MailboxId a) (MailboxId b) = compare a b

data Mailbox = Mailbox
  { mailboxId :: MailboxId,
    mailboxName :: T.Text,
    mailboxParentId :: Maybe MailboxId
  }
  deriving (Show, Generic, Data)

instance Aeson.FromJSON Mailbox where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "mailbox"

instance Aeson.ToJSON Mailbox where
  toJSON = Aeson.genericToJSON $ aesonOptionWithLabelPrefix "mailbox"

mailboxFullName :: [Mailbox] -> Mailbox -> T.Text
mailboxFullName mailboxes Mailbox {mailboxName = name, mailboxParentId = Nothing} = name
mailboxFullName mailboxes Mailbox {mailboxName = name, mailboxParentId = parent_id} =
  case find (\m -> mailboxId m == fromJust parent_id) mailboxes of
    Nothing -> name
    Just parent -> mailboxFullName mailboxes parent <> T.pack "/" <> name

findMailboxByFullName :: [Mailbox] -> T.Text -> Maybe Mailbox
findMailboxByFullName mailboxes name = find (\m -> mailboxFullName mailboxes m == name) mailboxes

makeGetMailboxMethodCall :: T.Text -> [(T.Text, MethodCallArg)] -> MethodCall
makeGetMailboxMethodCall id args =
  MethodCall
    { methodCallCapability = MailCapability,
      methodCallName = "Mailbox/get",
      methodCallArgs = methodCallArgsFrom updated_args,
      methodCallId = id
    }
  where
    updated_args =
      ("properties", methodCallArgFrom $ fieldLabels "mailbox" (undefined :: Mailbox)) : args

newtype EmailId = EmailId T.Text
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show, Data, Eq, Ord, Aeson.FromJSONKey, Aeson.ToJSONKey)

newtype EmailKeyword = EmailKeyword T.Text
  deriving (Aeson.ToJSON, Aeson.FromJSON, Aeson.FromJSONKey, Aeson.ToJSONKey, Eq, Ord, Show, Data)

data Email = Email
  { emailId :: EmailId,
    emailBlobId :: BlobId,
    emailMailboxIds :: Map.Map MailboxId Bool,
    emailKeywords :: Map.Map EmailKeyword Bool,
    emailSize :: Int
  }
  deriving (Show, Generic, Data)

instance Aeson.FromJSON Email where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "email"

instance Aeson.ToJSON Email where
  toJSON = Aeson.genericToJSON $ aesonOptionWithLabelPrefix "email"

makeGetEmailMethodCall :: T.Text -> [(T.Text, MethodCallArg)] -> MethodCall
makeGetEmailMethodCall id args =
  MethodCall
    { methodCallCapability = MailCapability,
      methodCallName = "Email/get",
      methodCallArgs = methodCallArgsFrom updated_args,
      methodCallId = id
    }
  where
    updated_args =
      ("properties", methodCallArgFrom $ fieldLabels "email" (undefined :: Email)) :
      ("bodyProperties", methodCallArgFrom ([] :: [String])) :
      args

makeQueryEmailMethodCall :: T.Text -> [(T.Text, MethodCallArg)] -> MethodCall
makeQueryEmailMethodCall id args =
  MethodCall
    { methodCallCapability = MailCapability,
      methodCallName = "Email/query",
      methodCallId = id,
      methodCallArgs = methodCallArgsFrom args
    }
