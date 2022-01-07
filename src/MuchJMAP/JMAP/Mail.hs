{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MuchJMAP.JMAP.Mail where

import GHC.Generics
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Text as Text
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)

import MuchJMAP.JMAP.Core ( aesonOptionWithLabelPrefix
                          , getPrimaryAccount
                          , MethodCall(..)
                          , Capability(..)
                          , Request(..)
                          , MethodCallArg(..)
                          , methodCallArgFrom
                          , MethodCallArgs(..)
                          , CommonGetResponseBody(..)
                          , methodCallArgsFrom)
import MuchJMAP.JMAP.API ( RequestContext
                         , apiRequest
                         , parseResponseBody0)

newtype MailboxId = MailboxId String
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show, Aeson.FromJSONKey, Eq)

instance Ord MailboxId where
  compare (MailboxId a) (MailboxId b) = compare a b

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
  method_response <- parseResponseBody0 "call0" api_response
  return $ getResponseList method_response
  where req = MethodCall { methodCallCapability = MailCapability
                         , methodCallName = "Mailbox/get"
                         , methodCallId = "call0"
                         , methodCallArgs = args }
        args = methodCallArgsFrom
          [("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)]


newtype EmailId = EmailId String
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show)

newtype BlobId = BlobId String
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show)

data Email = Email { emailId :: EmailId
                   , emailBlobId :: BlobId
                   , emailMailboxIds :: Map MailboxId Bool
                   , emailKeywords :: Map String Bool  -- TODO
                   , emailSize :: Int
                   , emailSubject :: Text.Text}
             deriving (Show, Generic)

instance Aeson.FromJSON Email where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "email"

getAllEmail :: (MonadIO m, MonadThrow m) => RequestContext -> m [Email]
getAllEmail (config, session) = do
  api_response <- apiRequest (config, session) (Request [query_call, get_call])
  method_response <- parseResponseBody0 "get_call" api_response
  return $ getResponseList method_response
  where query_call = MethodCall { methodCallCapability = MailCapability
                                , methodCallName = "Email/query"
                                , methodCallId = "query_call"
                                , methodCallArgs = methodCallArgsFrom query_args}
        query_args = [ ("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)
                     , ("limit", methodCallArgFrom (10 :: Int))]
        get_call = MethodCall { methodCallCapability = MailCapability
                              , methodCallName = "Email/get"
                              , methodCallId = "get_call"
                              , methodCallArgs = methodCallArgsFrom get_args}
        get_args = [ ("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)
                   , ("ids", ResultReference query_call "/ids")]
