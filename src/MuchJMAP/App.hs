{-# LANGUAGE OverloadedStrings #-}

module MuchJMAP.App where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow, throwM, Exception)
import qualified Data.Text as T
import Data.Maybe
import qualified System.Log.Logger as Logger

import Network.JMAP.API ( RequestContext
                         , apiRequest)
import Network.JMAP.Mail ( Mailbox
                         , MailboxId(..)
                         , Email
                         , EmailId(..)
                         , BlobId(..)
                         , makeGetMailboxMethodCall
                         , makeGetEmailMethodCall
                         , makeQueryEmailMethodCall)
import Network.JMAP.Core ( methodCallResponse'
                         , methodCallArgFrom
                         , getPrimaryAccount
                         , CommonQueryResponseBody(..)
                         , MethodCallArg(..)
                         , Capability(..)
                         , Request(..)
                         , CommonGetResponseBody(..)
                         , QueryState)

infoM = Logger.infoM "App"
warningM = Logger.warningM "App"
errorM = Logger.errorM "App"

getAllMailbox :: (MonadIO m, MonadThrow m) => RequestContext -> m [Mailbox]
getAllMailbox (config, session) = do
  api_response <- apiRequest (config, session) (Request [call])
  case methodCallResponse' "call_0" api_response of
    Left _ -> return []
    Right call_response -> return $ getResponseList call_response
  where call = makeGetMailboxMethodCall "call_0" args
        args = [("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)]


data JMAPException = JMAPException T.Text
  deriving (Show)

instance Exception JMAPException


fullQueryEmailIds :: (MonadIO m, MonadThrow m) => RequestContext -> Maybe [MailboxId] -> m [EmailId]
fullQueryEmailIds = fullQueryEmailIdsWithOffset Nothing 0

fullQueryEmailIdsWithOffset :: (MonadIO m, MonadThrow m) =>
  Maybe QueryState -> Int -> RequestContext -> Maybe [MailboxId] -> m [EmailId]
fullQueryEmailIdsWithOffset last_state offset (config, session) mailboxes = do
  liftIO $ infoM $
    "Running full query for email IDs from " ++
    (case mailboxes of
       Just m -> show $ length m
       Nothing -> "all") ++ " mailboxes, offset " ++ show offset
  api_response <- apiRequest (config, session) (Request [query_call])
  case methodCallResponse' "query_call" api_response of
    Left err -> do
      liftIO $ errorM $ show err
      throwM $ JMAPException "Failed to run Email/query"
    Right method_response -> handleResponse method_response
  where handleResponse resp
          | isJust last_state && fromJust last_state /= queryResponseQueryState resp = do
              liftIO $ warningM "QueryState changed, start over"
              fullQueryEmailIds (config, session) mailboxes
          | null (queryResponseIds resp) = return []
          | otherwise = do
              let result_ids = queryResponseIds resp
              liftIO $ infoM $ "Server returned " ++ show (length result_ids) ++ " email IDs"
              next_result_ids <- fullQueryEmailIdsWithOffset
                (Just $ queryResponseQueryState resp)
                (offset + length result_ids)
                (config, session) mailboxes
              return $ result_ids ++ next_result_ids
        query_call = makeQueryEmailMethodCall "query_call" call_args
        call_args =
          [ ("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)
          , ("position", methodCallArgFrom offset)] ++
          case mailboxes of Just ms -> [("inMailbox", methodCallArgFrom ms)]
                            Nothing -> []

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
