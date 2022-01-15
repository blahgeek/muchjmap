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
                          , CommonQueryResponseBody(..)
                          , FilterCondition(..)
                          , SortComparator(..)
                          , aesonOptionWithLabelPrefix
                          , fieldLabels
                          , getPrimaryAccount
                          , methodCallResponse
                          , methodCallResponse'
                          , MethodCallError(..)
                          , QueryState(..)
                          , AccountId(..)
                          , BlobId(..)
                          ) where

import qualified Data.Set as Set
import qualified Data.Text as T
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

data Capability = CoreCapability | MailCapability | CustomCapability T.Text
  deriving (Show, Eq)

capabilityNames :: [(T.Text, Capability)]
capabilityNames = [("urn:ietf:params:jmap:core", CoreCapability),
                   ("urn:ietf:params:jmap:mail", MailCapability)]

capabilityName :: Capability -> T.Text
capabilityName (CustomCapability s) = s
capabilityName c = fst $ fromJust $ find (\x -> c == snd x) capabilityNames

fromCapabilityName :: T.Text -> Capability
fromCapabilityName s = case lookup s capabilityNames of Just c -> c
                                                        Nothing -> CustomCapability s

instance Aeson.ToJSON Capability where
  toJSON c = Aeson.toJSON $ capabilityName c

instance Aeson.FromJSON Capability where
  parseJSON v = Aeson.parseJSON v <&> fromCapabilityName

instance Aeson.FromJSONKey Capability where
  fromJSONKey = Aeson.FromJSONKeyText fromCapabilityName

instance Ord Capability where
  (<=) a b = capabilityName a <= capabilityName b

newtype AccountId = AccountId T.Text
  deriving (Aeson.ToJSON, Aeson.FromJSON, Aeson.FromJSONKey, Eq, Ord, Show)

newtype BlobId = BlobId T.Text
  deriving (Aeson.ToJSON, Aeson.FromJSON, Show, Data)

data SessionResourceAccount = SessionResourceAccount { accountName :: T.Text}
                              deriving (Show, Generic, Eq)

instance Aeson.FromJSON SessionResourceAccount where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "account"

data SessionResource = SessionResource { sessionAccounts :: Map AccountId SessionResourceAccount
                                       , sessionApiUrl :: T.Text
                                       , sessionDownloadUrl :: T.Text
                                       , sessionUsername :: T.Text
                                       , sessionPrimaryAccounts :: Map Capability AccountId
                                       }
                       deriving (Show, Generic, Eq)

instance Aeson.FromJSON SessionResource where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "session"


getPrimaryAccount :: SessionResource -> Capability -> AccountId
getPrimaryAccount session c =
  findWithDefault ((head . keys . sessionAccounts) session) c (sessionPrimaryAccounts session)

-- Request & Response

data MethodCallArg = MethodCallArg Aeson.Value |
                     ResultReference MethodCall T.Text  -- path
                     deriving (Show)

methodCallArgFrom :: (Aeson.ToJSON a) => a -> MethodCallArg
methodCallArgFrom x = MethodCallArg $ Aeson.toJSON x

newtype MethodCallArgs = MethodCallArgs (Map T.Text MethodCallArg)
                         deriving (Show)

methodCallArgsFrom x = MethodCallArgs (fromList x)

data MethodCall = MethodCall { methodCallCapability :: Capability
                             , methodCallName :: T.Text
                             , methodCallArgs :: MethodCallArgs
                             , methodCallId :: T.Text }
                  deriving (Show)

instance Aeson.ToJSON MethodCallArgs where
  toJSON (MethodCallArgs m) =
    Aeson.object $ map toEntry (toList m)
    where toEntry (key, MethodCallArg val) = key .= val
          toEntry (key, ResultReference call path) =
            T.cons '#' key .= Aeson.object [ "resultOf" .= methodCallId call
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

data MethodResponse = MethodResponse { methodResponseName :: T.Text
                                     , methodResponseBody :: Aeson.Value
                                     , methodResponseId :: T.Text }
                      deriving (Show)

instance Aeson.FromJSON MethodResponse where
  parseJSON s =
    convert <$> (Aeson.parseJSON s :: Aeson.Types.Parser (T.Text, Aeson.Value, T.Text))
    where convert (name, body, id) = MethodResponse{ methodResponseName = name
                                                   , methodResponseBody = body
                                                   , methodResponseId = id}

data Response = Response { responseMethodResponses :: [MethodResponse]
                         , responseSessionState :: T.Text }
                   deriving (Show, Generic)

instance Aeson.FromJSON Response where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "response"


-- parse response

data MethodCallError = ParseBodyError Aeson.Value |
                       MethodCallResponseNoIndexError Int |
                       MethodCallError Aeson.Value
  deriving (Show, Eq)

methodCallResponse ::
  Aeson.FromJSON a =>
  Int ->     -- idx of response with same id
  T.Text ->  -- id
  Response ->
  Either MethodCallError a
methodCallResponse i id response
  | i >= length matched = Left $ MethodCallResponseNoIndexError i
  | methodResponseName (matched !! i) == "error" =
    Left $ MethodCallError $ methodResponseBody (matched !! i)
  | otherwise =
    let body = methodResponseBody (matched !! i) in
      case Aeson.Types.parse Aeson.parseJSON body of
        (Aeson.Success val) -> Right val
        (Aeson.Error _) -> Left $ ParseBodyError body
  where matched = filter
          (\method_response -> methodResponseId method_response == id)
          (responseMethodResponses response)

methodCallResponse' ::
  (Aeson.FromJSON a) =>
  T.Text ->  -- id
  Response ->
  Either MethodCallError a
methodCallResponse' = methodCallResponse 0

-- Filter & sort

data FilterCondition = FilterValue Aeson.Value |
                       FilterOpAND [FilterCondition] |
                       FilterOpOR [FilterCondition] |
                       FilterOpNOT [FilterCondition]
                       deriving (Show, Eq)

instance Aeson.ToJSON FilterCondition where
  toJSON (FilterValue val) = val
  toJSON (FilterOpAND conds) = Aeson.object [ "operator" .= ("AND" :: String)
                                            , "conditions" .= Aeson.toJSON conds]
  toJSON (FilterOpOR conds) = Aeson.object [ "operator" .= ("OR" :: String)
                                           , "conditions" .= Aeson.toJSON conds]
  toJSON (FilterOpNOT conds) = Aeson.object [ "operator" .= ("NOT" :: String)
                                            , "conditions" .= Aeson.toJSON conds]

data SortComparator = SortAscending T.Text | SortDescending T.Text
                      deriving (Show, Eq)

instance Aeson.ToJSON SortComparator where
  toJSON (SortAscending s) = Aeson.object [ "property" .= s
                                          , "isAscending" .= True]
  toJSON (SortDescending s) = Aeson.object [ "property" .= s
                                           , "isAscending" .= False]

-- some common response data

newtype GetState = GetState T.Text
  deriving (Aeson.ToJSON, Aeson.FromJSON, Aeson.FromJSONKey, Eq, Ord, Show)

data CommonGetResponseBody a = CommonGetResponseBody{ getResponseAccountId :: AccountId
                                                    , getResponseState :: GetState
                                                    , getResponseList :: [a]
                                                    , getResponseNotFound :: [T.Text]}
                               deriving (Show, Generic)

instance (Aeson.FromJSON a) => Aeson.FromJSON (CommonGetResponseBody a) where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "getResponse"

newtype QueryState = QueryState T.Text
  deriving (Aeson.ToJSON, Aeson.FromJSON, Aeson.FromJSONKey, Eq, Ord, Show)

data CommonQueryResponseBody a = CommonQueryResponseBody{ queryResponseAccountId :: AccountId
                                                        , queryResponseQueryState :: QueryState
                                                        , queryResponseCanCalculateChanges :: Bool
                                                        , queryResponsePosition :: Int
                                                        , queryResponseIds :: [a]
                                                        , queryResponseTotal :: Maybe Int
                                                        , queryResponseLimit :: Maybe Int}
                                 deriving (Show, Generic)

instance (Aeson.FromJSON a) => Aeson.FromJSON (CommonQueryResponseBody a) where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "queryResponse"
