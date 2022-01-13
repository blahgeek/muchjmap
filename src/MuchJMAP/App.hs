{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MuchJMAP.App where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow, throwM, Exception)
import qualified Data.Text as T
import Data.Maybe
import qualified System.Log.Logger as Logger
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import GHC.Generics

import Network.JMAP.API ( RequestContext
                        , apiRequest
                        , ServerConfig(..))
import Network.JMAP.Mail ( Mailbox(..)
                         , MailboxId(..)
                         , Email
                         , EmailId(..)
                         , makeGetMailboxMethodCall
                         , makeGetEmailMethodCall
                         , makeQueryEmailMethodCall
                         , findMailboxByFullName)
import Network.JMAP.Core ( methodCallResponse'
                         , methodCallArgFrom
                         , getPrimaryAccount
                         , aesonOptionWithLabelPrefix
                         , CommonQueryResponseBody(..)
                         , MethodCallArg(..)
                         , Capability(..)
                         , Request(..)
                         , BlobId(..)
                         , CommonGetResponseBody(..)
                         , QueryState
                         , FilterCondition(..))

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

data EmailFilter = EmailFilter { emailFilterMailboxes :: Maybe [T.Text]}
  deriving (Show, Generic)

instance Aeson.FromJSON EmailFilter where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "emailFilter"

encodeEmailFilter :: [Mailbox] -> EmailFilter -> FilterCondition
encodeEmailFilter mailboxes EmailFilter {emailFilterMailboxes=mailbox_names} =
  FilterOpAND (op_in_mailbox mailbox_names)
  where op_in_mailbox Nothing = []
        op_in_mailbox (Just names) =
          let ids = map (mailboxId . fromJust) $ filter isJust $ map (findMailboxByFullName mailboxes) names in
            [FilterOpOR $ map (\m -> FilterValue $ Aeson.object ["inMailbox" .= m]) ids]

data Config = Config { configServerConfig :: ServerConfig
                     , configEmailFilter :: EmailFilter }
                deriving (Show, Generic)

instance Aeson.FromJSON Config where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "config"

data EmailIdsSyncState = EmailIdsSyncState { emailIdsQueryState :: Maybe QueryState
                                           , emailIds :: [EmailId] }
                         deriving (Show)

queryEmailIdsFull :: (MonadIO m, MonadThrow m) =>
  RequestContext -> FilterCondition -> m EmailIdsSyncState
queryEmailIdsFull = queryEmailIdsFullWithOffset Nothing 0

queryEmailIdsFullWithOffset :: (MonadIO m, MonadThrow m) =>
  Maybe QueryState -> Int -> RequestContext -> FilterCondition -> m EmailIdsSyncState
queryEmailIdsFullWithOffset last_state offset (config, session) filters = do
  liftIO $ infoM $
    "Running full query for email IDs, offset " ++ show offset ++ ", filters: " ++ show filters
  api_response <- apiRequest (config, session) (Request [query_call])
  case methodCallResponse' "query_call" api_response of
    Left err -> do
      liftIO $ errorM $ show err
      throwM $ JMAPException "Failed to run Email/query"
    Right method_response -> handleResponse method_response
  where handleResponse resp
          | isJust last_state && fromJust last_state /= queryResponseQueryState resp = do
              liftIO $ warningM "QueryState changed, start over"
              queryEmailIdsFull (config, session) filters
          | null (queryResponseIds resp) =
            return EmailIdsSyncState{ emailIdsQueryState = Just $ queryResponseQueryState resp
                                    , emailIds = []}
          | otherwise = do
              let result_ids = queryResponseIds resp
              let query_state = queryResponseQueryState resp
              liftIO $ infoM $ "Server returned " ++ show (length result_ids) ++ " email IDs"
              EmailIdsSyncState{emailIds=next_result_ids} <-
                queryEmailIdsFullWithOffset
                        (Just $ queryResponseQueryState resp)
                        (offset + length result_ids)
                        (config, session) filters
              return EmailIdsSyncState{ emailIdsQueryState = Just query_state
                                      , emailIds = result_ids ++ next_result_ids}
        query_call = makeQueryEmailMethodCall "query_call" call_args
        call_args =
          [ ("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)
          , ("position", methodCallArgFrom offset)
          , ("filter", methodCallArgFrom filters)]

-- queryEmailIdsDelta :: (MonadIO m, MonadThrow m) =>
--   RequestContext -> Maybe [MailboxId] -> EmailIdsSyncState -> m EmailIdsSyncState


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
