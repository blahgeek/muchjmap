module MuchJMAP.Download (downloadAllEmails) where

import Control.Monad (forM_, forM, when)
import Control.Monad.Catch (Exception, throwM, MonadCatch, catchAll)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.Log.Logger as Logger
import System.FilePath (takeFileName, (</>))
import System.Directory (removeFile, renameFile)

import Network.JMAP.Mail
  ( Email(..) , EmailId (EmailId)
  )
import Network.JMAP.Core
  ( BlobId(..) , getPrimaryAccount, Capability (MailCapability)
  )
import Network.JMAP.API
  ( RequestContext,
    ServerConfig (..),
    downloadBlob,
  )
import MuchJMAP.Maildir
import Control.Concurrent (newQSem, newEmptyMVar, takeMVar, forkIO, waitQSem, signalQSem, putMVar)
import Conduit (runResourceT)
import Data.List (isSuffixOf)


infoM = Logger.infoM "Download"
errorM = Logger.errorM "Download"

data DownloadException = DownloadException T.Text
  deriving (Show)

instance Exception DownloadException

kDownloadConcurrentJobs :: Int
kDownloadConcurrentJobs = 8

-- kEmailFilenameSuffix :: String
-- kEmailFilenameSuffix = ".eml"

emailFilename :: Email -> FilePath
emailFilename Email {emailId = EmailId id} = T.unpack id

emailIdFromPath :: FilePath -> EmailId
emailIdFromPath path = EmailId $ T.pack $ takeFileName path


findToDownloadAndToDelete :: [Email] -> Maildir -> IO ([Email], [FilePath])
findToDownloadAndToDelete emails maildir = do
  existing_files <- listMails maildir
  let existing_ids = Set.fromList $ map emailIdFromPath existing_files
      to_download = filter (\eml -> Set.notMember (emailId eml) existing_ids) emails
      expected_ids = Set.fromList $ map emailId emails
      to_delete_files = filter (\path -> Set.notMember (emailIdFromPath path) expected_ids) existing_files
  return (to_download, to_delete_files)

doDownload :: RequestContext -> Maildir -> Email -> IO FilePath
doDownload (config, session) maildir email = do
  infoM $ "Downloading email " ++ show (emailId email)
  let tmp_path = maildirTmpPath maildir </> emailFilename email
      dest_path = maildirNewPath maildir </> emailFilename email
  runResourceT $
    downloadBlob
      (config, session)
      (getPrimaryAccount session MailCapability)
      (emailBlobId email)
      tmp_path
  renameFile tmp_path dest_path
  infoM $ "Downloaded email " ++ show (emailId email) ++ " to " ++ dest_path
  return dest_path

-- ensure all emails are downloaded to maildir, and all unexpected emails are removed from maildir
downloadAllEmails :: RequestContext -> Maildir -> [Email] -> IO ()
downloadAllEmails context maildir emails = do
  createMaildirIfMissing maildir
  (to_download_emails, to_delete_paths) <- findToDownloadAndToDelete emails maildir

  forM_ to_delete_paths (\file -> do
    infoM $ "Deleting file " ++ file
    removeFile file)

  infoM $ "Start downloading " ++ show (length to_download_emails) ++ " emails"
  sem <- newQSem kDownloadConcurrentJobs
  job_mvars <- forM to_download_emails $ \email -> do
    mvar <- newEmptyMVar
    forkIO $ do
      waitQSem sem
      catchAll (doDownload context maildir email >> putMVar mvar True) $ \e -> do
        errorM $ "Error during downloading: " ++ show e
        putMVar mvar False
      signalQSem sem
    return mvar
  job_results <- forM job_mvars takeMVar
  when (any not job_results) $ throwM $ DownloadException $ T.pack $
    "Failed to download " ++ show (length . filter not $ job_results) ++ " files"
