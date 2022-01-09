{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.JMAP.Mail where

import GHC.Generics
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Data (Data)

import Network.JMAP.Core ( aesonOptionWithLabelPrefix
                          , fieldLabels
                          , getPrimaryAccount
                          , MethodCall(..)
                          , Capability(..)
                          , Request(..)
                          , Response
                          , MethodCallArg(..)
                          , methodCallArgFrom
                          , MethodCallArgs(..)
                          , CommonGetResponseBody(..)
                          , methodCallArgsFrom)

newtype MailboxId = MailboxId String
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show, Aeson.FromJSONKey, Eq, Data)

instance Ord MailboxId where
  compare (MailboxId a) (MailboxId b) = compare a b

data WellknownMailboxRole = RoleAll | RoleArchive | RoleDrafts | RoleFlagged |
                            RoleImportant | RoleInbox | RoleJunk | RoleSent |
                            RoleSubscribed | RoleTrash
  deriving (Show, Generic, Data)

instance Aeson.FromJSON WellknownMailboxRole where
  parseJSON = Aeson.genericParseJSON $
                Aeson.defaultOptions{
                        Aeson.constructorTagModifier =
                          map toLower . drop 4}  -- "Role"

data MailboxRole = WellknownRole WellknownMailboxRole | UnknownRole String
  deriving (Show, Generic, Data)

instance Aeson.FromJSON MailboxRole where
  parseJSON = Aeson.genericParseJSON $
                Aeson.defaultOptions{Aeson.sumEncoding = Aeson.UntaggedValue}

data Mailbox = Mailbox { mailboxId :: MailboxId
                       , mailboxName :: String
                       , mailboxParentId :: Maybe MailboxId
                       , mailboxRole :: Maybe MailboxRole }
               deriving (Show, Generic, Data)

instance Aeson.FromJSON Mailbox where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "mailbox"


makeGetMailboxMethodCall :: String -> [(String, MethodCallArg)] -> MethodCall
makeGetMailboxMethodCall id args =
  MethodCall { methodCallCapability = MailCapability
             , methodCallName = "Mailbox/get"
             , methodCallArgs = methodCallArgsFrom updated_args
             , methodCallId = id}
  where updated_args =
          ("properties", methodCallArgFrom $ fieldLabels "mailbox" (undefined :: Mailbox)) : args


newtype EmailId = EmailId String
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show, Data)

newtype BlobId = BlobId String
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show, Data)

data Email = Email { emailId :: EmailId
                   , emailBlobId :: BlobId
                   , emailMailboxIds :: Map.Map MailboxId Bool
                   , emailKeywords :: Map.Map String Bool  -- TODO
                   , emailSize :: Int
                   , emailSubject :: Text.Text}
             deriving (Show, Generic, Data)

instance Aeson.FromJSON Email where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "email"

makeGetEmailMethodCall :: String -> [(String, MethodCallArg)] -> MethodCall
makeGetEmailMethodCall id args =
  MethodCall { methodCallCapability = MailCapability
             , methodCallName = "Email/get"
             , methodCallArgs = methodCallArgsFrom updated_args
             , methodCallId = id}
  where updated_args =
          ("properties", methodCallArgFrom $ fieldLabels "email" (undefined :: Email)) : args

makeQueryEmailMethodCall :: String -> [(String, MethodCallArg)] -> MethodCall
makeQueryEmailMethodCall id args =
  MethodCall { methodCallCapability = MailCapability
             , methodCallName = "Email/query"
             , methodCallId = id
             , methodCallArgs = methodCallArgsFrom args}
