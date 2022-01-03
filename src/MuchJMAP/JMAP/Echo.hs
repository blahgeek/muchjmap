{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MuchJMAP.JMAP.Echo ( echo
                          ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import GHC.Generics

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import MuchJMAP.JMAP.Core ( MethodResponse(..)
                          , MethodCall(..)
                          , Request(..)
                          , Response(..)
                          , Capability(..)
                          )
import MuchJMAP.JMAP.API ( RequestContext
                         , apiRequest
                         , parseResponseBody0)

-- Core/echo
makeEchoMethodCall :: (Aeson.ToJSON args) => String -> args -> MethodCall
makeEchoMethodCall id args = MethodCall { methodCallCapability = CoreCapability
                                        , methodCallName = "Core/echo"
                                        , methodCallArgs = Aeson.toJSON args
                                        , methodCallId = id }

newtype EchoArgs = EchoArgs { echoMessage :: String }
  deriving (Show, Generic)

instance Aeson.ToJSON EchoArgs
instance Aeson.FromJSON EchoArgs

echo :: (MonadIO m, MonadThrow m) => RequestContext -> String -> m String
echo context msg =
  let method_call = makeEchoMethodCall "id0" (EchoArgs{echoMessage = msg}) in do
    response <- apiRequest context (Request [method_call])
    liftIO $ print response
    body <- parseResponseBody0 "id0" response
    return $ echoMessage body
