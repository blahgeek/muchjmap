module MuchJMAP.App where

import Control.Monad (when)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Maybe
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import Network.JMAP.API
  ( RequestContext,
    ServerConfig (..),
    apiRequest, getSessionResource
  )
import Network.JMAP.Core
  ( BlobId (..),
    Capability (..),
    SessionResource (..),
    CommonGetResponseBody (..),
    CommonQueryResponseBody (..),
    FilterCondition (..),
    MethodCallArg (..),
    QueryState,
    GetState,
    Request (..),
    aesonOptionWithLabelPrefix,
    getPrimaryAccount,
    methodCallArgFrom,
    methodCallResponse',
  )
import Network.JMAP.Mail
  ( Email,
    EmailId (..),
    Mailbox (..),
    MailboxId (..),
    findMailboxByFullName,
    makeGetEmailMethodCall,
    makeGetMailboxMethodCall,
    makeQueryEmailMethodCall,
  )
import qualified System.Log.Logger as Logger

infoM = Logger.infoM "App"

warningM = Logger.warningM "App"

errorM = Logger.errorM "App"

kMaxGetPerCall :: Int
kMaxGetPerCall = 500

data JMAPException = JMAPException T.Text
  deriving (Show)

instance Exception JMAPException

data EmailFilter = EmailFilter {emailFilterMailboxes :: Maybe [T.Text]}
  deriving (Show, Generic)

instance Aeson.FromJSON EmailFilter where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "emailFilter"

encodeEmailFilter :: [Mailbox] -> EmailFilter -> FilterCondition
encodeEmailFilter mailboxes EmailFilter {emailFilterMailboxes = mailbox_names} =
  FilterOpAND (op_in_mailbox mailbox_names)
  where
    op_in_mailbox Nothing = []
    op_in_mailbox (Just names) =
      let ids = map (mailboxId . fromJust) $ filter isJust $ map (findMailboxByFullName mailboxes) names
       in [FilterOpOR $ map (\m -> FilterValue $ Aeson.object ["inMailbox" .= m]) ids]

data Config = Config
  { configServerConfig :: ServerConfig,
    configEmailFilter :: EmailFilter
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Config where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "config"


data SyncState = SyncState
  { syncStateSession :: SessionResource,
    syncStateMailboxes :: [Mailbox],
    syncStateMailboxesState :: Maybe GetState,
    syncStateEmails :: Map EmailId (Maybe Email),
    syncStateEmailIdsState :: Maybe QueryState
  }
  deriving (Show, Generic)

instance Aeson.FromJSON SyncState where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "syncState"

instance Aeson.ToJSON SyncState where
  toJSON = Aeson.genericToJSON $ aesonOptionWithLabelPrefix "syncState"

syncMailboxes :: (MonadIO m, MonadThrow m) => Config -> SyncState -> m SyncState
syncMailboxes
  config
  sync_state@SyncState
    { syncStateSession = session,
      syncStateMailboxesState = Nothing
    } = do
    api_response <- apiRequest (configServerConfig config, session) (Request [call])
    case methodCallResponse' "call_0" api_response of
      Left err -> do
        liftIO $ errorM $ show err
        throwM $ JMAPException "Failed to sync mailboxes"
      Right call_response -> do
        return $
          sync_state
            { syncStateMailboxes = getResponseList call_response,
              syncStateMailboxesState = Just $ getResponseState call_response
            }
    where
      call = makeGetMailboxMethodCall "call_0" args
      args = [("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)]
syncMailboxes _ _ = error "syncMailboxes should only work when mailboxesState is empty"

-- full sync emails & emailIdsState using Email/query
-- used when emails & emailIdsState is empty
syncEmailsFull ::
  (MonadIO m, MonadThrow m) =>
  Config ->
  SyncState ->
  m SyncState
syncEmailsFull
  config
  sync_state@SyncState{syncStateEmailIdsState=Nothing} =
  syncEmailsFullWithOffset 0 config sync_state{syncStateEmails=Map.empty}
syncEmailsFull _ _ = error "syncEmailsFull should only work when emailIdsState is empty"

syncEmailsFullWithOffset ::
  (MonadIO m, MonadThrow m) =>
  Int ->
  Config ->
  SyncState ->
  m SyncState
syncEmailsFullWithOffset
  offset
  config@Config
    { configServerConfig = server_config,
      configEmailFilter = filters
    }
  sync_state@SyncState
    { syncStateSession = session,
      syncStateMailboxes = mailboxes,
      syncStateEmails = old_emails,
      syncStateEmailIdsState = old_query_state
    } = do
    liftIO $
      infoM $
        "Running full query for email IDs, offset " ++ show offset ++ ", filters: " ++ show filter_condition
    api_response <- apiRequest (server_config, session) (Request [query_call])
    case methodCallResponse' "query_call" api_response of
      Left err -> do
        liftIO $ errorM $ show err
        throwM $ JMAPException "Failed to run Email/query"
      Right method_response -> handleResponse method_response
    where
      handleResponse resp
        | isJust old_query_state && fromJust old_query_state /= queryResponseQueryState resp = do
          liftIO $ warningM "QueryState changed, start over"
          syncEmailsFull config
            sync_state{
                syncStateEmails = Map.empty,
                syncStateEmailIdsState = Nothing}
        | null (queryResponseIds resp) =
          return sync_state {syncStateEmailIdsState = Just $ queryResponseQueryState resp}
        | otherwise = do
          let result_ids = queryResponseIds resp
          liftIO $ infoM $ "Server returned " ++ show (length result_ids) ++ " email IDs"
          let new_state =
                sync_state {syncStateEmails =
                            Map.union old_emails
                            (Map.fromList (map (\id -> (id, Nothing)) result_ids))}
          syncEmailsFullWithOffset (offset + length result_ids) config new_state
      query_call = makeQueryEmailMethodCall "query_call" call_args
      call_args =
        [ ("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability),
          ("position", methodCallArgFrom offset),
          ("filter", methodCallArgFrom filter_condition),
          ("limit", methodCallArgFrom kMaxGetPerCall)
        ]
      filter_condition =
        encodeEmailFilter mailboxes filters


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


runSync :: (MonadIO m, MonadThrow m) => Config -> m SyncState
runSync config = do
  -- todo: input sync_state
  session <- getSessionResource (configServerConfig config)
  let sync_state = SyncState{ syncStateSession = session,
                            syncStateMailboxes = [],
                            syncStateMailboxesState = Nothing,
                            syncStateEmails = Map.empty,
                            syncStateEmailIdsState = Nothing}
  syncMailboxes config sync_state >>= syncEmailsFull config
