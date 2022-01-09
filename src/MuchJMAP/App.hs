module MuchJMAP.App where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow)

import Network.JMAP.API ( RequestContext
                         , apiRequest)
import Network.JMAP.Mail ( Mailbox
                         , Email
                         , makeGetMailboxMethodCall
                         , makeGetEmailMethodCall
                         , makeQueryEmailMethodCall)
import Network.JMAP.Core ( methodCallResponse'
                         , methodCallArgFrom
                         , getPrimaryAccount
                         , MethodCallArg(..)
                         , Capability(..)
                         , Request(..)
                         , CommonGetResponseBody(..))

getAllMailbox :: (MonadIO m, MonadThrow m) => RequestContext -> m [Mailbox]
getAllMailbox (config, session) = do
  api_response <- apiRequest (config, session) (Request [call])
  case methodCallResponse' "call_0" api_response of
    Left _ -> return []
    Right call_response -> return $ getResponseList call_response
  where call = makeGetMailboxMethodCall "call_0" args
        args = [("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)]


getAllEmail :: (MonadIO m, MonadThrow m) => RequestContext -> m [Email]
getAllEmail (config, session) = do
  api_response <- apiRequest (config, session) (Request [query_call, get_call])
  case methodCallResponse' "get_call" api_response of
    Left _ -> return []
    Right method_response -> return $ getResponseList method_response
  where query_call = makeQueryEmailMethodCall "query_call"
                        [ ("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)
                        , ("limit", methodCallArgFrom (10 :: Int))]
        get_call = makeGetEmailMethodCall "get_call"
                        [ ("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)
                        , ("ids", ResultReference query_call "/ids")]
