{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.JMAP.Echo ( echo
                          ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import GHC.Generics

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Network.JMAP.Core ( MethodResponse(..)
                          , MethodCall(..)
                          , Request(..)
                          , Response(..)
                          , Capability(..)
                          , MethodCallArgs
                          , methodCallArgsFrom
                          , methodCallArgFrom
                          , MethodCallArg (ResultReference)
                          )
import Network.JMAP.API ( RequestContext
                         , apiRequest
                         , parseResponseBody0)

-- Core/echo
makeEchoMethodCall :: String -> MethodCallArgs -> MethodCall
makeEchoMethodCall id args = MethodCall { methodCallCapability = CoreCapability
                                        , methodCallName = "Core/echo"
                                        , methodCallArgs = args
                                        , methodCallId = id }

newtype EchoArgs = EchoArgs { echoMessage :: String }
  deriving (Show, Generic)

instance Aeson.FromJSON EchoArgs

echo :: (MonadIO m, MonadThrow m) => RequestContext -> String -> m String
echo context msg = do
  response <- apiRequest context (Request [call0, call1])
  liftIO $ print response
  body <- parseResponseBody0 "call1" response
  return $ echoMessage body
  where call0 = makeEchoMethodCall "call0" $ methodCallArgsFrom
                        [("arg0", methodCallArgFrom msg)]
        call1 = makeEchoMethodCall "call1" $ methodCallArgsFrom
                        [("echoMessage", ResultReference call0 "/arg0")]
