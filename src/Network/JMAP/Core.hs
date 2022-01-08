{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.JMAP.Core ( SessionResourceAccount(..)
                          , SessionResource(..)
                          , Capability(..)
                          , MethodCallArg(..)
                          , methodCallArgFrom
                          , MethodCallArgs(..)
                          , methodCallArgsFrom
                          , MethodCall(..)
                          , MethodResponse(..)
                          , Request(..)
                          , Response(..)
                          , CommonGetResponseBody(..)
                          , aesonOptionWithLabelPrefix
                          , fieldLabels
                          , getPrimaryAccount
                          ) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List (isPrefixOf, find)
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Data.ByteString (ByteString)
import Data.Map (Map, findWithDefault, keys, toList, fromList)
import Data.Functor
import Data.Data
import GHC.Generics

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Control.Monad.Catch (MonadThrow)

-- simple helpers

dropAndLowerFirst :: String -> String -> String
dropAndLowerFirst prefix name =
  if prefix `isPrefixOf` name then lowerFirst $ drop (length prefix) name else name
  where lowerFirst [] = []
        lowerFirst (s:xs) = toLower s : xs

aesonOptionWithLabelPrefix :: String -> Aeson.Options
aesonOptionWithLabelPrefix prefix = Aeson.defaultOptions {Aeson.fieldLabelModifier = dropAndLowerFirst prefix}

fieldLabels :: (Data a) => String -> a -> [String]  -- return field labels but remove prefix
fieldLabels prefix a =
  let constr_fields = map constrFields . dataTypeConstrs . dataTypeOf $ a in
    map (dropAndLowerFirst prefix) (head constr_fields)

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

data SessionResourceAccount = SessionResourceAccount { accountName :: String}
                              deriving (Show, Generic, Eq)

instance Aeson.FromJSON SessionResourceAccount where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "account"

data SessionResource = SessionResource { sessionAccounts :: Map String SessionResourceAccount
                                       , sessionApiUrl :: String
                                       , sessionDownloadUrl :: String
                                       , sessionUsername :: String
                                       , sessionPrimaryAccounts :: Map Capability String
                                       }
                       deriving (Show, Generic, Eq)

instance Aeson.FromJSON SessionResource where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "session"


getPrimaryAccount :: SessionResource -> Capability -> String
getPrimaryAccount session c =
  findWithDefault ((head . keys . sessionAccounts) session) c (sessionPrimaryAccounts session)

-- Request & Response

data MethodCallArg = MethodCallArg Aeson.Value |
                     ResultReference MethodCall String  -- path
                     deriving (Show)

methodCallArgFrom :: (Aeson.ToJSON a) => a -> MethodCallArg
methodCallArgFrom x = MethodCallArg $ Aeson.toJSON x

newtype MethodCallArgs = MethodCallArgs (Map String MethodCallArg)
                         deriving (Show)

methodCallArgsFrom x = MethodCallArgs (fromList x)

data MethodCall = MethodCall { methodCallCapability :: Capability
                             , methodCallName :: String
                             , methodCallArgs :: MethodCallArgs
                             , methodCallId :: String }
                  deriving (Show)

instance Aeson.ToJSON MethodCallArgs where
  toJSON (MethodCallArgs m) =
    Aeson.object $ map toEntry (toList m)
    where toEntry (key, MethodCallArg val) = Text.pack key .= val
          toEntry (key, ResultReference call path) =
            Text.pack ('#' : key) .= Aeson.object [ "resultOf" .= methodCallId call
                                                  , "name" .= methodCallName call
                                                  , "path" .= path]

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


data CommonGetResponseBody a = CommonGetResponseBody{ getResponseAccountId :: String
                                                    , getResponseState :: String
                                                    , getResponseList :: [a]
                                                    , getResponseNotFound :: [String]}
                               deriving (Show, Generic)

instance (Aeson.FromJSON a) => Aeson.FromJSON (CommonGetResponseBody a) where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "getResponse"
