{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MuchJMAP.JMAP.Mail where

import GHC.Generics
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)

import MuchJMAP.JMAP.Core ( aesonOptionWithLabelPrefix
                          , getPrimaryAccount
                          , MethodCall(..)
                          , Capability(..)
                          , Request(..)
                          , CommonGetRequestArgs(..)
                          , CommonGetResponseBody(..)
                          , defaultCommonGetRequestArgs)
import MuchJMAP.JMAP.API ( RequestContext
                         , apiRequest
                         , parseResponseBody0)

newtype MailboxId = MailboxId String
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show)

data WellknownMailboxRole = RoleAll | RoleArchive | RoleDrafts | RoleFlagged |
                            RoleImportant | RoleInbox | RoleJunk | RoleSent |
                            RoleSubscribed | RoleTrash
  deriving (Show, Generic)

instance Aeson.FromJSON WellknownMailboxRole where
  parseJSON = Aeson.genericParseJSON $
                Aeson.defaultOptions{
                        Aeson.constructorTagModifier =
                          map toLower . drop 4}  -- "Role"

data MailboxRole = WellknownRole WellknownMailboxRole | UnknownRole String
  deriving (Show, Generic)

instance Aeson.FromJSON MailboxRole where
  parseJSON = Aeson.genericParseJSON $
                Aeson.defaultOptions{Aeson.sumEncoding = Aeson.UntaggedValue}

data Mailbox = Mailbox { mailboxId :: MailboxId
                       , mailboxName :: String
                       , mailboxParentId :: Maybe MailboxId
                       , mailboxRole :: Maybe MailboxRole }
               deriving (Show, Generic)

instance Aeson.FromJSON Mailbox where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "mailbox"

getAllMailbox :: (MonadIO m, MonadThrow m) => RequestContext -> m [Mailbox]
getAllMailbox (config, session) = do
  api_response <- apiRequest (config, session) (Request [req])
  method_response <- parseResponseBody0 "id0" api_response
  return $ getResponseList method_response
  where req = MethodCall {
          methodCallCapability = MailCapability
        , methodCallName = "Mailbox/get"
        , methodCallId = "id0"
        , methodCallArgs = Aeson.toJSON defaultCommonGetRequestArgs{
            getRequestAccountId=getPrimaryAccount session MailCapability}}
