{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MuchJMAP.JMAP.Mail where

import GHC.Generics
import qualified Data.Aeson as Aeson

import MuchJMAP.JMAP.Core (aesonOptionWithLabelPrefix)

newtype MailboxId = MailboxId String
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show)

data Mailbox = Mailbox { mailboxId :: MailboxId
                       , mailboxName :: String
                       , mailboxParentId :: MailboxId
                       , mailboxRole :: String }
               deriving (Show, Generic)

instance Aeson.FromJSON Mailbox where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "mailbox"
