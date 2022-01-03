{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module MuchJMAP.JMAP.Types ( SessionResourceAccount(..)
                           , SessionResource(..)
                           , MethodCall
                           , MethodResponse
                           , Request(..)
                           , Response
                           , makeEchoMethodCall
                           ) where

import qualified Data.Set as Set
import Data.List (isPrefixOf)
import Data.Char (toLower)
import Data.ByteString (ByteString)
import Data.Map (Map)
import GHC.Generics

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

aesonOptionWithLabelPrefix :: String -> Aeson.Options
aesonOptionWithLabelPrefix prefix = Aeson.defaultOptions {Aeson.fieldLabelModifier = mod}
  where mod name = if prefix `isPrefixOf` name then lowerFirst $ drop (length prefix) name else name
        lowerFirst [] = []
        lowerFirst (s:xs) = toLower s : xs

-- Session

data SessionResourceAccount = SessionResourceAccount { accountName :: String
                                                     , accountUserId :: String}
                              deriving (Show, Generic)

instance Aeson.FromJSON SessionResourceAccount where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "account"

data SessionResource = SessionResource { sessionAccounts :: Map String SessionResourceAccount
                                       , sessionApiUrl :: String
                                       , sessionDownloadUrl :: String
                                       , sessionUsername :: String}
                       deriving (Show, Generic)

instance Aeson.FromJSON SessionResource where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "session"


-- Request & Response

data Capability = CoreCapability | MailCapability | CustomCapability String
  deriving (Show)

toString :: Capability -> String
toString CoreCapability = "urn:ietf:params:jmap:core"
toString MailCapability = "urn:ietf:params:jmap:mail"
toString (CustomCapability s) = s

instance Aeson.ToJSON Capability where
  toJSON c = Aeson.toJSON $ toString c

instance Eq Capability where
  (==) a b = toString a == toString b

instance Ord Capability where
  (<=) a b = toString a <= toString b

data MethodCall = MethodCall { methodCallCapability :: Capability
                             , methodCallName :: String
                             , methodCallArgs :: Aeson.Value
                             , methodCallId :: String }
                  deriving (Show)

instance Aeson.ToJSON MethodCall where
  toJSON MethodCall{methodCallCapability=_, methodCallName=name, methodCallArgs=args, methodCallId=id} =
    Aeson.toJSON (name, args, id)

newtype Request = Request [MethodCall] deriving (Show)

instance Aeson.ToJSON Request where
  toJSON (Request calls) =
    Aeson.object ["using" .= capabilities,
                  "methodCalls" .= calls]
    where capabilities = Set.toList $ Set.fromList $ map methodCallCapability calls

data MethodResponse = MethodResponse { methodResponseName :: String
                                     , methodResponseBody :: Aeson.Value
                                     , methodResponseId :: String }
                      deriving (Show)

parseMethodResponseBody :: (Aeson.FromJSON a) => MethodResponse -> Aeson.Result a
parseMethodResponseBody resp = Aeson.Types.parse Aeson.parseJSON (methodResponseBody resp)

instance Aeson.FromJSON MethodResponse where
  parseJSON s =
    convert <$> (Aeson.parseJSON s :: Aeson.Types.Parser (String, Aeson.Value, String))
    where convert (name, body, id) = MethodResponse{ methodResponseName = name
                                                   , methodResponseBody = body
                                                   , methodResponseId = id}

data Response = Response { responseMethodResponses :: [MethodResponse]
                         , responseSessionState :: String }
                   deriving (Show, Generic)

instance Aeson.FromJSON Response where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "response"

-- Core
makeEchoMethodCall :: (Aeson.ToJSON args) => String -> args -> MethodCall
makeEchoMethodCall id args = MethodCall { methodCallCapability = CoreCapability
                                        , methodCallName = "Core/echo"
                                        , methodCallArgs = Aeson.toJSON args
                                        , methodCallId = id }

-- Mail

newtype MailboxId = MailboxId String
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show)

data Mailbox = Mailbox { mailboxId :: MailboxId
                       , mailboxName :: String
                       , mailboxParentId :: MailboxId
                       , mailboxRole :: String }
               deriving (Show, Generic)

instance Aeson.FromJSON Mailbox where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "mailbox"
