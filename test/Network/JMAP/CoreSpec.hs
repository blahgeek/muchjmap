{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.JMAP.CoreSpec where

import Test.Hspec
import Data.Data
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.FileEmbed

import Network.JMAP.Core

data TestData = TestData { testHello :: String
                         , testWorldXyz :: Int }
                deriving (Show, Data)

spec :: Spec
spec = do
  describe "fieldLabels" $ do
    it "should work for TestData" $ do
      fieldLabels "test" (undefined :: TestData) `shouldBe` ["hello", "worldXyz"]

  describe "capabilities" $ do
    it "should serialize to json" $ do
      Aeson.encode CoreCapability `shouldBe` C.pack "\"urn:ietf:params:jmap:core\""
      Aeson.encode MailCapability `shouldBe` C.pack "\"urn:ietf:params:jmap:mail\""
      Aeson.encode (CustomCapability "hello:world") `shouldBe` C.pack "\"hello:world\""
    it "should parse from json" $ do
      Aeson.decode (C.pack "\"urn:ietf:params:jmap:core\"") `shouldBe` Just CoreCapability
      Aeson.decode (C.pack "\"urn:ietf:params:jmap:mail\"") `shouldBe` Just MailCapability
      Aeson.decode (C.pack "\"asdf\"") `shouldBe` Just (CustomCapability "asdf")

  describe "session resource" $ do
    let session_context = Aeson.decode (C.fromStrict $(embedFile "test/Network/JMAP/data/session_resource.json"))
    it "should decode from json" $ do
      session_context `shouldBe`
        Just SessionResource{ sessionAccounts = Map.fromList [ ("A13824", SessionResourceAccount{ accountName = "john@example.com"})
                                                             , ("A97813", SessionResourceAccount{ accountName = "jane@example.com"})]
                            , sessionApiUrl = "https://jmap.example.com/api/"
                            , sessionDownloadUrl =  "https://jmap.example.com/download/{accountId}/{blobId}/{name}?accept={type}"
                            , sessionUsername = "john@example.com"
                            , sessionPrimaryAccounts = Map.fromList [ (MailCapability, "A13824")
                                                                    , (CustomCapability "urn:ietf:params:jmap:contacts", "A13824")]}
    it "can get primary account" $ do
      getPrimaryAccount (fromJust session_context) MailCapability `shouldBe` "A13824"

  describe "request data" $ do
    let request_json_str = C.fromStrict $(embedFile "test/Network/JMAP/data/foo_request.json")
        request_value = Aeson.decode request_json_str :: Maybe Aeson.Value
        call_0 = MethodCall{ methodCallCapability = CoreCapability
                           , methodCallName = "Foo/changes"
                           , methodCallId = "t0"
                           , methodCallArgs = methodCallArgsFrom [ ("accountId", methodCallArgFrom ("A1" :: String))
                                                                 , ("sinceState", methodCallArgFrom ("abcdef" :: String))]}
        call_1 = MethodCall{ methodCallCapability = CoreCapability
                           , methodCallName = "Foo/get"
                           , methodCallId = "t1"
                           , methodCallArgs = methodCallArgsFrom [ ("accountId", methodCallArgFrom ("A1" :: String))
                                                                 , ("ids", ResultReference call_0 "/created")]}
        request = Request [call_0, call_1]
    it "should serialize to json" $ do
      Aeson.toJSON request `shouldBe` Aeson.object [ "using" .= (["urn:ietf:params:jmap:core"] :: [String])
                                                   , "methodCalls" .= request_value]

