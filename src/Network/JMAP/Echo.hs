module Network.JMAP.Echo
  ( echo,
  )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Either
import qualified Data.Text as T
import GHC.Generics
import Network.JMAP.API
  ( RequestContext,
    apiRequest,
  )
import Network.JMAP.Core
  ( Capability (..),
    MethodCall (..),
    MethodCallArg (ResultReference),
    MethodCallArgs,
    MethodResponse (..),
    Request (..),
    Response (..),
    methodCallArgFrom,
    methodCallArgsFrom,
    methodCallResponse',
  )

-- Core/echo
makeEchoMethodCall :: T.Text -> MethodCallArgs -> MethodCall
makeEchoMethodCall id args =
  MethodCall
    { methodCallCapability = CoreCapability,
      methodCallName = "Core/echo",
      methodCallArgs = args,
      methodCallId = id
    }

newtype EchoArgs = EchoArgs {echoMessage :: T.Text}
  deriving (Show, Generic)

instance Aeson.FromJSON EchoArgs

echo :: (MonadIO m, MonadThrow m) => RequestContext -> T.Text -> m T.Text
echo context msg = do
  response <- apiRequest context (Request [call0, call1])
  liftIO $ print response
  case methodCallResponse' "call1" response of
    Left _ -> return ""
    Right body -> return $ echoMessage body
  where
    call0 =
      makeEchoMethodCall "call0" $
        methodCallArgsFrom
          [("arg0", methodCallArgFrom msg)]
    call1 =
      makeEchoMethodCall "call1" $
        methodCallArgsFrom
          [("echoMessage", ResultReference call0 "/arg0")]
