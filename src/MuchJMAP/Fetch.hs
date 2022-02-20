module MuchJMAP.Fetch (fetchEmailsFull, fetchEmailsDelta, FetchState(..)) where

import Control.Monad (when)
import Control.Monad.Catch (Exception, MonadThrow, throwM, catch, MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Maybe
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import qualified System.Log.Logger as Logger
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
    CommonQueryChangesResponseBody (..),
    CommonChangesResponseBody (..),
    FilterCondition (..),
    MethodCallArg (..),
    QueryState,
    GetState,
    Request (..),
    MethodCallError,
    aesonOptionWithLabelPrefix,
    getPrimaryAccount,
    methodCallArgFrom,
    methodCallResponse', methodCallResponse
  )
import Network.JMAP.Mail
  ( Email(..) ,
    EmailId (..),
    Mailbox (..),
    MailboxId (..),
    findMailboxByFullName,
    makeEmailGetMethodCall,
    makeGetMailboxMethodCall,
    makeEmailQueryMethodCall,
    makeEmailQueryChangesMethodCall,
    makeEmailChangesMethodCall,
  )
import MuchJMAP.Config

infoM = Logger.infoM "Fetch"

warningM = Logger.warningM "Fetch"

errorM = Logger.errorM "Fetch"

kMaxGetPerCall :: Int
kMaxGetPerCall = 500

newtype FetchException = FetchException T.Text
  deriving (Show)

instance Exception FetchException

fromRightOrException :: (MonadIO m, MonadThrow m, Show e) => Either e a -> m a
fromRightOrException (Left err) = do
  throwM $ FetchException (T.pack (show err))
fromRightOrException (Right val) = return val

encodeEmailFilter :: (MonadThrow m) => FetchState -> EmailFilter -> m FilterCondition
encodeEmailFilter
  FetchState {fetchStateMailboxes = all_mailboxes}
  EmailFilter {emailFilterMailboxes = mailbox_names} =
    FilterOpAND <$> op_in_mailbox mailbox_names
    where
      op_in_mailbox Nothing = return []
      op_in_mailbox (Just names) = do
        mailboxes <- mapM findMailbox names
        return
          [ FilterOpOR $
              map
                ( \m ->
                    FilterValue $
                      Aeson.object ["inMailbox" .= mailboxId m]
                )
                mailboxes
          ]
      findMailbox name = case findMailboxByFullName all_mailboxes name of
        Nothing -> throwM $ FetchException $ T.pack "Unknown mailbox name " <> name
        (Just v) -> return v

data FetchState = FetchState
  { fetchStateSession :: SessionResource,
    fetchStateMailboxes :: [Mailbox],
    fetchStateEmailIdsState :: Maybe QueryState,
    fetchStateEmailPropsState :: Maybe GetState
  }
  deriving (Show, Generic)

instance Aeson.FromJSON FetchState where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "fetchState"

instance Aeson.ToJSON FetchState where
  toJSON = Aeson.genericToJSON $ aesonOptionWithLabelPrefix "fetchState"

initializeFetchState :: (MonadIO m, MonadThrow m) => ServerConfig -> m FetchState
initializeFetchState server_config = do
  liftIO $ infoM "Initializing fetch state..."
  session <- getSessionResource server_config
  liftIO $ infoM "Fetching mailboxes..."
  let call =
        makeGetMailboxMethodCall
          "call_0"
          [("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)]
  api_response <- apiRequest (server_config, session) (Request [call])
  call_response <- fromRightOrException $ methodCallResponse' "call_0" api_response
  liftIO $
    infoM $
      "Fetch mailboxes done. Got " ++ show (length (getResponseList call_response)) ++ " mailboxes"
  return $
    FetchState
      { fetchStateSession = session,
        fetchStateMailboxes = getResponseList call_response,
        fetchStateEmailIdsState = Nothing,
        fetchStateEmailPropsState = Nothing
      }

-- full fetch emails & emailIdsState using Email/query
-- used when emails & emailIdsState is empty
fetchEmailsFull :: (MonadIO m, MonadThrow m) => Config -> m ([Email], FetchState)
fetchEmailsFull config =
  initializeFetchState (configServerConfig config) >>=
  \state -> fetchEmailFull' 0 [] state config

fetchEmailFull' ::
  (MonadIO m, MonadThrow m) =>
  Int ->
  [Email] ->
  FetchState ->
  Config ->
  m ([Email], FetchState)
fetchEmailFull'
  offset
  old_emails
  old_fetch_state@FetchState
    { fetchStateSession = session,
      fetchStateMailboxes = mailboxes,
      fetchStateEmailIdsState = old_query_state,
      fetchStateEmailPropsState = old_get_state
    }
  config@Config
    { configServerConfig = server_config,
      configEmailFilter = filters
    } = do
    filter_condition <- encodeEmailFilter old_fetch_state filters
    liftIO $
      infoM $
        "Fetching emails (full query), offset " ++ show offset ++ ", filters: " ++ show filter_condition

    let query_call =
          makeEmailQueryMethodCall
            "query_call"
            [ ("accountId", methodCallArgFrom account_id),
              ("position", methodCallArgFrom offset),
              ("filter", methodCallArgFrom filter_condition),
              ("limit", methodCallArgFrom kMaxGetPerCall)
            ]
        get_call =
          makeEmailGetMethodCall
            "get_call"
            [ ("accountId", methodCallArgFrom account_id),
              ("ids", ResultReference query_call "/ids")
            ]
    api_response <- apiRequest (server_config, session) (Request [query_call, get_call])

    query_resp <- fromRightOrException $ methodCallResponse' "query_call" api_response
    let query_state = queryResponseQueryState (query_resp :: CommonQueryResponseBody EmailId)
        query_result_ids = queryResponseIds query_resp

    if isJust old_query_state && fromJust old_query_state /= query_state
      then do
        liftIO $ warningM "QueryState changed, start over"
        fetchEmailsFull config
      else
        if null query_result_ids
          then do
            liftIO $ infoM "Fetch emails done."
            return (old_emails, old_fetch_state)
          else do
            get_method_response <- fromRightOrException $ methodCallResponse' "get_call" api_response
            liftIO $ infoM $ "Got " ++ show (length (getResponseList get_method_response)) ++ " emails"
            let new_emails = getResponseList get_method_response ++ old_emails
                new_fetch_state =
                  old_fetch_state
                    { fetchStateEmailPropsState =
                        Just $
                          -- props state: set to the state of first email/get response
                          fromMaybe (getResponseState get_method_response) old_get_state
                    }
            fetchEmailFull' (offset + length query_result_ids) new_emails new_fetch_state config
    where
      account_id = getPrimaryAccount session MailCapability

data FetchEmailsDeltaResult = FetchEmailsDeltaResult
  { deltaRemoved :: [EmailId],
    deltaAdded :: [Email],
    -- deltaUpdated may contain unmatched results
    -- caller should apply deltaRemoved, deltaAdded and deltaUpdated in order
    deltaUpdated :: [Email] }
  deriving (Show)

fetchEmailsDelta ::
  (MonadIO m, MonadThrow m) =>
  Config ->
  FetchState ->
  m (FetchEmailsDeltaResult, FetchState)
fetchEmailsDelta
  config@Config
    { configServerConfig = server_config,
      configEmailFilter = filters
    }
  old_fetch_state@FetchState
    { fetchStateSession = session,
      fetchStateMailboxes = mailboxes,
      fetchStateEmailIdsState = old_query_state,
      fetchStateEmailPropsState = old_get_state
    } = do
    filter_condition <- encodeEmailFilter old_fetch_state filters
    liftIO $ infoM $ "Fetching emails delta, filters: " ++ show filter_condition

    let query_changes_call =
          makeEmailQueryChangesMethodCall
            "query_changes_call"
            [ ("accountId", methodCallArgFrom account_id),
              ("filter", methodCallArgFrom filter_condition),
              ("sinceQueryState", methodCallArgFrom old_query_state)
            ]
        get_added_call =
          makeEmailGetMethodCall
            "get_added_call"
            [ ("accountId", methodCallArgFrom account_id),
              ("ids", ResultReference query_changes_call "/added/*/id")
            ]
        -- all new emails must be included in query_changes_call/get_added_call
        -- because query_state must be equal or newer than get_state
        -- we only need to get modified emails using "changes" call
        changes_call =
          makeEmailChangesMethodCall
            "changes_call"
            [ ("accountId", methodCallArgFrom account_id),
              ("sinceState", methodCallArgFrom old_get_state)
            ]
        get_modified_call =
          makeEmailGetMethodCall
            "get_modified_call"
            [ ("accountId", methodCallArgFrom account_id),
              ("ids", ResultReference changes_call "/updated")
            ]
    api_response <-
      apiRequest
        (server_config, session)
        (Request [query_changes_call, get_added_call, changes_call, get_modified_call])

    query_resp <- fromRightOrException $ methodCallResponse' "query_changes_call" api_response
    get_added_resp <- fromRightOrException $ methodCallResponse' "get_added_call" api_response
    changes_resp <- fromRightOrException $ methodCallResponse' "changes_call" api_response
    get_modified_resp <- fromRightOrException $ methodCallResponse' "get_modified_call" api_response
    let new_query_state = queryChangesResponseNewQueryState (query_resp :: CommonQueryChangesResponseBody EmailId)
        new_get_state = changesResponseNewState (changes_resp :: CommonChangesResponseBody EmailId)
        removed_ids = queryChangesResponseRemoved query_resp
        added_items = queryChangesResponseAdded query_resp
        modified_ids = changesResponseUpdated changes_resp
        new_fetch_state =
          old_fetch_state
            { fetchStateEmailIdsState = Just new_query_state,
              fetchStateEmailPropsState = Just new_get_state
            }
    liftIO $ infoM $ "Got " ++ show (length removed_ids) ++ " removed emails, " ++ show (length added_items) ++ " added emails from queryChanges"
    liftIO $ infoM $ "Got " ++ show (length modified_ids) ++ " modified emails from all changes"

    return
      ( FetchEmailsDeltaResult
          { deltaRemoved = removed_ids,
            deltaAdded = getResponseList get_added_resp,
            deltaUpdated = getResponseList get_modified_resp
          },
        new_fetch_state
      )
    where
      account_id = getPrimaryAccount session MailCapability
