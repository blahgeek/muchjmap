module MuchJMAP.Download where

import Control.Monad (forM_, forM, when)
import Control.Monad.Catch (Exception, throwM, MonadCatch, catchAll)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.Log.Logger as Logger

import Network.JMAP.Mail
  ( Email(..) ,
  )
import Network.JMAP.Core
  ( BlobId(..) , getPrimaryAccount, Capability (MailCapability)
  )
import Network.JMAP.API
  ( RequestContext,
    ServerConfig (..),
    downloadBlob,
  )
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
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

kEmailFilenameSuffix :: String
kEmailFilenameSuffix = ".eml"

emailFilename :: Email -> FilePath
emailFilename Email {emailBlobId = BlobId id} = T.unpack id ++ kEmailFilenameSuffix

-- ensure all emails are downloaded to directory
downloadAllEmails :: RequestContext -> FilePath -> [Email] -> IO ()
downloadAllEmails (config, session) dir emails = do
  createDirectoryIfMissing True dir
  existing_files <-
    Set.fromList . filter (isSuffixOf kEmailFilenameSuffix) <$> listDirectory dir
  let expected_files = Set.fromList $ map emailFilename emails
  forM_ (Set.difference existing_files expected_files) $ \file -> do
    infoM $ "Deleting file " ++ file
    removeFile (dir ++ "/" ++ file)

  let to_download_emails =
        filter (\email -> Set.notMember (emailFilename email) existing_files) emails
  infoM $ "Start downloading " ++ show (length to_download_emails) ++ " emails"
  sem <- newQSem kDownloadConcurrentJobs
  job_mvars <- forM to_download_emails $ \email -> do
    mvar <- newEmptyMVar
    forkIO $ do
      waitQSem sem
      catchAll (doDownload email >> putMVar mvar True) $ \e -> do
        errorM $ "Error during downloading: " ++ show e
        putMVar mvar False
      signalQSem sem
    return mvar
  job_results <- forM job_mvars takeMVar
  when (any not job_results) $ throwM $ DownloadException $ T.pack $
    "Failed to download " ++ show (length . filter not $ job_results) ++ " files"
  where
    doDownload email = do
      infoM $ "Downloading email " ++ emailFilename email
      runResourceT $
        downloadBlob
          (config, session)
          (getPrimaryAccount session MailCapability)
          (emailBlobId email)
          (dir ++ "/" ++ emailFilename email)
