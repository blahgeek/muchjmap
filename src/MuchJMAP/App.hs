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
import qualified System.Log.Logger as Logger
import System.Directory (doesFileExist)
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
    MethodCallError,
    aesonOptionWithLabelPrefix,
    getPrimaryAccount,
    methodCallArgFrom,
    methodCallResponse',
  )
import Network.JMAP.Mail
  ( Email(..) ,
    EmailId (..),
    Mailbox (..),
    MailboxId (..),
    findMailboxByFullName,
    makeGetEmailMethodCall,
    makeGetMailboxMethodCall,
    makeQueryEmailMethodCall,
  )
import MuchJMAP.Config

infoM = Logger.infoM "App"

warningM = Logger.warningM "App"

errorM = Logger.errorM "App"

kMaxGetPerCall :: Int
kMaxGetPerCall = 500

data AppException = AppException T.Text
  deriving (Show)

instance Exception AppException

fromRight :: (MonadIO m, MonadThrow m) => Either MethodCallError a -> m a
fromRight (Left err) = do
  liftIO $ errorM $ show err
  throwM $ AppException "Failed in fromRight"
fromRight (Right val) = return val

data SyncState = SyncState
  { syncStateSession :: SessionResource,
    syncStateMailboxes :: [Mailbox],
    syncStateMailboxesState :: Maybe GetState,
    syncStateEmails :: Map EmailId Email,
    syncStateEmailIdsState :: Maybe QueryState,
    syncStateEmailPropsState :: Maybe GetState
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
    liftIO $ infoM "Syncing mailboxes..."
    api_response <- apiRequest (configServerConfig config, session) (Request [call])
    call_response <- fromRight $ methodCallResponse' "call_0" api_response
    liftIO $ infoM $
      "Sync mailboxes done. Got " ++ show (length (getResponseList call_response)) ++ " mailboxes"
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
  sync_state@SyncState{syncStateEmailIdsState=Nothing,
                      syncStateEmailPropsState=Nothing} =
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
      syncStateEmailIdsState = old_query_state,
      syncStateEmailPropsState = old_get_state
    } = do
    liftIO $
      infoM $
        "Syncing emails (full query), offset " ++ show offset ++ ", filters: " ++ show filter_condition
    api_response <- apiRequest (server_config, session) (Request [query_call, get_call])

    query_resp <- fromRight $ methodCallResponse' "query_call" api_response
    let query_state = queryResponseQueryState (query_resp :: CommonQueryResponseBody EmailId)
        query_result_ids = queryResponseIds query_resp

    if isJust old_query_state && fromJust old_query_state /= query_state
      then do
        liftIO $ warningM "QueryState changed, start over"
        syncEmailsFull
          config
          sync_state
            { syncStateEmails = Map.empty,
              syncStateEmailIdsState = Nothing
            }
      else
        if null query_result_ids
          then do
            liftIO $ infoM "Sync emails done."
            return sync_state {syncStateEmailIdsState = Just $ queryResponseQueryState query_resp}
          else do
            get_method_response <- fromRight $ methodCallResponse' "get_call" api_response
            new_sync_state <- handleGetResponse (get_method_response :: CommonGetResponseBody Email)
            syncEmailsFullWithOffset (offset + length query_result_ids) config new_sync_state
    where
      handleGetResponse resp = do
        liftIO $ infoM $ "Got " ++ show (length (getResponseList resp)) ++ " emails"
        let new_emails = Map.fromList $ map (\m -> (emailId m, m)) (getResponseList resp)
        return $
          sync_state
            { syncStateEmails = Map.union new_emails old_emails,
              syncStateEmailPropsState =
                if isNothing old_get_state
                  then Just (getResponseState resp)
                  else old_get_state
            }
      query_call =
        makeQueryEmailMethodCall
          "query_call"
          [ ("accountId", methodCallArgFrom account_id),
            ("position", methodCallArgFrom offset),
            ("filter", methodCallArgFrom filter_condition),
            ("limit", methodCallArgFrom kMaxGetPerCall)
          ]
      get_call =
        makeGetEmailMethodCall
          "get_call"
          [ ("accountId", methodCallArgFrom account_id),
            ("ids", ResultReference query_call "/ids")
          ]
      account_id = getPrimaryAccount session MailCapability
      filter_condition = encodeEmailFilter mailboxes filters


runSync :: (MonadIO m, MonadThrow m) => Config -> Maybe SyncState -> m SyncState
runSync config Nothing = do
  session <- getSessionResource (configServerConfig config)
  runSync config $
    Just
      SyncState
        { syncStateSession = session,
          syncStateMailboxes = [],
          syncStateMailboxesState = Nothing,
          syncStateEmails = Map.empty,
          syncStateEmailIdsState = Nothing,
          syncStateEmailPropsState = Nothing
        }
runSync config (Just sync_state@SyncState {syncStateMailboxesState = Nothing}) =
  syncMailboxes config sync_state >>= \s -> runSync config (Just s)
runSync
  config
  ( Just
      sync_state@SyncState
        { syncStateEmailIdsState = Nothing,
          syncStateEmailPropsState = Nothing
        }
    ) =
    syncEmailsFull config sync_state
-- TODO
runSync config (Just sync_state) = return sync_state


runApp :: (MonadIO m, MonadThrow m) => Config -> m ()
runApp config = do
  let sync_state_filepath = syncStateFilePath config
  sync_state <- liftIO $ do
    file_exists <- doesFileExist sync_state_filepath
    if file_exists then
      Aeson.decodeFileStrict sync_state_filepath
      else return Nothing
  sync_state <- runSync config sync_state
  liftIO $ Aeson.encodeFile (syncStateFilePath config) sync_state
