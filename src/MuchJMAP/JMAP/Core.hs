{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MuchJMAP.JMAP.Core ( SessionResourceAccount(..)
                          , SessionResource(..)
                          , Capability(..)
                          , MethodCall(..)
                          , MethodResponse(..)
                          , Request(..)
                          , Response(..)
                          , CommonGetRequestArgs(..)
                          , CommonGetResponseBody(..)
                          , defaultCommonGetRequestArgs
                          , aesonOptionWithLabelPrefix
                          , getPrimaryAccount
                          ) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List (isPrefixOf, find)
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Data.ByteString (ByteString)
import Data.Map (Map, findWithDefault, keys)
import Data.Functor
import GHC.Generics

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Control.Monad.Catch (MonadThrow)

aesonOptionWithLabelPrefix :: String -> Aeson.Options
aesonOptionWithLabelPrefix prefix = Aeson.defaultOptions {Aeson.fieldLabelModifier = mod}
  where mod name = if prefix `isPrefixOf` name then lowerFirst $ drop (length prefix) name else name
        lowerFirst [] = []
        lowerFirst (s:xs) = toLower s : xs

-- Session

data Capability = CoreCapability | MailCapability | CustomCapability String
  deriving (Show, Eq)

capabilityNames :: [(String, Capability)]
capabilityNames = [("urn:ietf:params:jmap:core", CoreCapability),
                   ("urn:ietf:params:jmap:mail", MailCapability)]

toString :: Capability -> String
toString (CustomCapability s) = s
toString c = fst $ fromJust $ find (\x -> c == snd x) capabilityNames

fromString :: String -> Capability
fromString s = case lookup s capabilityNames of Just c -> c
                                                Nothing -> CustomCapability s

instance Aeson.ToJSON Capability where
  toJSON c = Aeson.toJSON $ toString c

instance Aeson.FromJSON Capability where
  parseJSON v = (Aeson.parseJSON v :: Aeson.Types.Parser String) <&> fromString

instance Aeson.FromJSONKey Capability where
  fromJSONKey = Aeson.FromJSONKeyText convert
    where convert text = fromString (Text.unpack text)

instance Ord Capability where
  (<=) a b = toString a <= toString b

data SessionResourceAccount = SessionResourceAccount { accountName :: String
                                                     , accountUserId :: String}
                              deriving (Show, Generic)

instance Aeson.FromJSON SessionResourceAccount where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "account"

data SessionResource = SessionResource { sessionAccounts :: Map String SessionResourceAccount
                                       , sessionApiUrl :: String
                                       , sessionDownloadUrl :: String
                                       , sessionUsername :: String
                                       , sessionPrimaryAccounts :: Map Capability String
                                       }
                       deriving (Show, Generic)

instance Aeson.FromJSON SessionResource where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "session"


getPrimaryAccount :: SessionResource -> Capability -> String
getPrimaryAccount session c =
  findWithDefault ((head . keys . sessionAccounts) session) c (sessionPrimaryAccounts session)

-- Request & Response

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


data CommonGetRequestArgs = CommonGetRequestArgs{ getRequestAccountId :: String
                                                , getRequestIds :: Maybe [String]
                                                , getRequestProperties :: Maybe [String]}
                            deriving (Show, Generic)

defaultCommonGetRequestArgs = CommonGetRequestArgs{ getRequestAccountId = ""
                                                  , getRequestIds = Nothing
                                                  , getRequestProperties = Nothing}

instance Aeson.ToJSON CommonGetRequestArgs where
  toJSON = Aeson.genericToJSON $ aesonOptionWithLabelPrefix "getRequest"

data CommonGetResponseBody a = CommonGetResponseBody{ getResponseAccountId :: String
                                                    , getResponseState :: String
                                                    , getResponseList :: [a]
                                                    , getResponseNotFound :: [String]}
                               deriving (Show, Generic)

instance (Aeson.FromJSON a) => Aeson.FromJSON (CommonGetResponseBody a) where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "getResponse"
