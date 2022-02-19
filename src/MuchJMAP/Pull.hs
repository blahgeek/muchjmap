module MuchJMAP.Pull where

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

infoM = Logger.infoM "Pull"

warningM = Logger.warningM "Pull"

errorM = Logger.errorM "Pull"

kMaxGetPerCall :: Int
kMaxGetPerCall = 500

data PullException = PullException T.Text
  deriving (Show)

instance Exception PullException

fromRight :: (MonadIO m, MonadThrow m, Show e) => Either e a -> m a
fromRight (Left err) = do
  throwM $ PullException (T.pack (show err))
fromRight (Right val) = return val

data PullState = PullState
  { pullStateSession :: SessionResource,
    pullStateMailboxes :: [Mailbox],
    pullStateMailboxesState :: Maybe GetState,
    pullStateEmails :: Map EmailId Email,
    pullStateEmailIdsState :: Maybe QueryState,
    pullStateEmailPropsState :: Maybe GetState
  }
  deriving (Show, Generic)

instance Aeson.FromJSON PullState where
  parseJSON = Aeson.genericParseJSON $ aesonOptionWithLabelPrefix "pullState"

instance Aeson.ToJSON PullState where
  toJSON = Aeson.genericToJSON $ aesonOptionWithLabelPrefix "pullState"

pullMailboxes :: (MonadIO m, MonadThrow m) => Config -> PullState -> m PullState
pullMailboxes
  config
  pull_state@PullState
    { pullStateSession = session,
      pullStateMailboxesState = Nothing
    } = do
    liftIO $ infoM "Pulling mailboxes..."
    api_response <- apiRequest (configServerConfig config, session) (Request [call])
    call_response <- fromRight $ methodCallResponse' "call_0" api_response
    liftIO $ infoM $
      "Pull mailboxes done. Got " ++ show (length (getResponseList call_response)) ++ " mailboxes"
    return $
      pull_state
        { pullStateMailboxes = getResponseList call_response,
          pullStateMailboxesState = Just $ getResponseState call_response
        }
    where
      call = makeGetMailboxMethodCall "call_0" args
      args = [("accountId", methodCallArgFrom $ getPrimaryAccount session MailCapability)]
pullMailboxes _ _ = error "pullMailboxes should only work when mailboxesState is empty"

addToMap :: Ord k => [(k, v)] -> Map k v -> Map k v
addToMap elems = Map.union (Map.fromList elems)

deleteFromMap :: Ord k => [k] -> Map k v -> Map k v
deleteFromMap ids m = foldr Map.delete m ids

adjustInMap :: Ord k => [(k, v)] -> Map k v -> Map k v
adjustInMap elems orig_m = foldr (\(k, v) m -> Map.adjust (const v) k m) orig_m elems

-- full pull emails & emailIdsState using Email/query
-- used when emails & emailIdsState is empty
pullEmailsFull ::
  (MonadIO m, MonadThrow m) =>
  Config ->
  PullState ->
  m PullState
pullEmailsFull
  config
  pull_state@PullState{pullStateEmailIdsState=Nothing,
                      pullStateEmailPropsState=Nothing} =
  pullEmailsFullWithOffset 0 config pull_state{pullStateEmails=Map.empty}
pullEmailsFull _ _ = error "pullEmailsFull should only work when emailIdsState is empty"

pullEmailsFullWithOffset ::
  (MonadIO m, MonadThrow m) =>
  Int ->
  Config ->
  PullState ->
  m PullState
pullEmailsFullWithOffset
  offset
  config@Config
    { configServerConfig = server_config,
      configEmailFilter = filters
    }
  pull_state@PullState
    { pullStateSession = session,
      pullStateMailboxes = mailboxes,
      pullStateEmails = old_emails,
      pullStateEmailIdsState = old_query_state,
      pullStateEmailPropsState = old_get_state
    } = do
    liftIO $
      infoM $
        "Pulling emails (full query), offset " ++ show offset ++ ", filters: " ++ show filter_condition
    api_response <- apiRequest (server_config, session) (Request [query_call, get_call])

    query_resp <- fromRight $ methodCallResponse' "query_call" api_response
    let query_state = queryResponseQueryState (query_resp :: CommonQueryResponseBody EmailId)
        query_result_ids = queryResponseIds query_resp

    if isJust old_query_state && fromJust old_query_state /= query_state
      then do
        liftIO $ warningM "QueryState changed, start over"
        pullEmailsFull
          config
          pull_state
            { pullStateEmails = Map.empty,
              pullStateEmailIdsState = Nothing
            }
      else
        if null query_result_ids
          then do
            liftIO $ infoM "Pull emails done."
            return pull_state {pullStateEmailIdsState = Just $ queryResponseQueryState query_resp}
          else do
            get_method_response <- fromRight $ methodCallResponse' "get_call" api_response
            liftIO $ infoM $ "Got " ++ show (length (getResponseList get_method_response)) ++ " emails"
            let new_emails = Map.fromList $
                  map (\m -> (emailId m, m)) (getResponseList get_method_response)
                new_pull_state =
                  pull_state {pullStateEmails = Map.union new_emails old_emails,
                              pullStateEmailPropsState = Just $
                               -- props state: set to the state of first email/get response
                               fromMaybe (getResponseState get_method_response) old_get_state}
            pullEmailsFullWithOffset (offset + length query_result_ids) config new_pull_state
    where
      query_call =
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
      account_id = getPrimaryAccount session MailCapability
      filter_condition = encodeEmailFilter mailboxes filters

pullEmailsDelta :: (MonadIO m, MonadThrow m) => Config -> PullState -> m PullState
pullEmailsDelta
  config@Config
    { configServerConfig = server_config,
      configEmailFilter = filters
    }
  pull_state@PullState
    { pullStateSession = session,
      pullStateMailboxes = mailboxes,
      pullStateEmailIdsState = old_query_state,
      pullStateEmailPropsState = old_get_state,
      pullStateEmails = old_emails
    } = do
    liftIO $ infoM $ "Pulling emails (by changes), filters: " ++ show filters
    api_response <- apiRequest
      (server_config, session)
      (Request [query_changes_call, get_added_call, changes_call, get_modified_call])

    query_resp <- fromRight $ methodCallResponse' "query_changes_call" api_response
    get_added_resp <- fromRight $ methodCallResponse' "get_added_call" api_response
    changes_resp <- fromRight $ methodCallResponse' "changes_call" api_response
    get_modified_resp <- fromRight $ methodCallResponse' "get_modified_call" api_response
    let new_query_state = queryChangesResponseNewQueryState (query_resp :: CommonQueryChangesResponseBody EmailId)
        new_get_state = changesResponseNewState (changes_resp :: CommonChangesResponseBody EmailId)
        removed_ids = queryChangesResponseRemoved query_resp
        added_items = queryChangesResponseAdded query_resp
        modified_ids = changesResponseUpdated changes_resp
    liftIO $ infoM $ "Got " ++ show (length removed_ids) ++ " removed emails, " ++ show (length added_items) ++ " added emails from queryChanges"
    liftIO $ infoM $ "Got " ++ show (length modified_ids) ++ " modified emails from all changes"

    return $ pull_state
      { pullStateEmailIdsState = Just new_query_state,
        pullStateEmailPropsState = Just new_get_state,
        pullStateEmails =
          adjustInMap (map (\m -> (emailId m, m)) (getResponseList get_modified_resp)) .
          addToMap (map (\m -> (emailId m, m)) (getResponseList get_added_resp)) .
          deleteFromMap removed_ids $
          old_emails}
    where
      query_changes_call =
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
            ("sinceState", methodCallArgFrom old_get_state)]
      get_modified_call =
        makeEmailGetMethodCall
          "get_modified_call"
          [ ("accountId", methodCallArgFrom account_id),
            ("ids", ResultReference changes_call "/updated")
          ]
      account_id = getPrimaryAccount session MailCapability
      filter_condition = encodeEmailFilter mailboxes filters

runPull :: (MonadIO m, MonadThrow m, MonadCatch m) => Config -> Maybe PullState -> m PullState
runPull
  config
  ( Just
      pull_state@PullState
        { pullStateMailboxesState = Just _,
          pullStateEmailIdsState = Just _,
          pullStateEmailPropsState = Just _
        }
    ) = do
    liftIO $ infoM "Running partial pull..."
    catch
      (pullEmailsDelta config pull_state)
      ( \e -> do
          liftIO $ errorM (show (e :: PullException))
          liftIO $ warningM "Partial pull failed, fallback to full pull."
          runPull config Nothing
      )
runPull config _ = do
  liftIO $ infoM "Running full pull..."
  session <- getSessionResource (configServerConfig config)
  let initial_pull_state =
        PullState
          { pullStateSession = session,
            pullStateMailboxes = [],
            pullStateMailboxesState = Nothing,
            pullStateEmails = Map.empty,
            pullStateEmailIdsState = Nothing,
            pullStateEmailPropsState = Nothing
          }
  pullMailboxes config initial_pull_state
    >>= \s -> pullEmailsFull config s
