module MuchJMAP.Download (emailFilename, downloadEmails, emailIdFromFilePath) where

import Control.Monad (forM_, forM, when, unless)
import Control.Monad.Catch (Exception, throwM, MonadCatch, catchAll)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.Log.Logger as Logger
import System.FilePath ((</>), takeBaseName)

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
import Control.Concurrent (newQSem, newEmptyMVar, takeMVar, forkIO, waitQSem, signalQSem, putMVar)
import Conduit (runResourceT)
import Data.List (isSuffixOf)
import System.Directory (createDirectoryIfMissing, doesFileExist)


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
emailFilename Email {emailId = EmailId id} = T.unpack id ++ kEmailFilenameSuffix

emailIdFromFilePath :: FilePath -> EmailId
emailIdFromFilePath = EmailId . T.pack . takeBaseName

doDownload :: RequestContext -> FilePath -> Email -> IO FilePath
doDownload (config, session) dir email = do
  let dest_path = dir </> emailFilename email
  dest_path_exists <- doesFileExist dest_path
  unless dest_path_exists $ do
    infoM $ "Downloading email " ++ show (emailId email)
    runResourceT $
      downloadBlob
        (config, session)
        (getPrimaryAccount session MailCapability)
        (emailBlobId email)
        dest_path
    infoM $ "Downloaded email " ++ show (emailId email) ++ " to " ++ dest_path
  return dest_path

downloadEmails :: RequestContext -> FilePath -> [Email] -> IO ()
downloadEmails context dir emails = do
  createDirectoryIfMissing True dir
  infoM $ "Start downloading " ++ show (length emails) ++ " emails"
  sem <- newQSem kDownloadConcurrentJobs
  job_mvars <- forM emails $ \email -> do
    mvar <- newEmptyMVar
    forkIO $ do
      waitQSem sem
      catchAll (doDownload context dir email >> putMVar mvar True) $ \e -> do
        errorM $ "Error during downloading: " ++ show e
        putMVar mvar False
      signalQSem sem
    return mvar
  job_results <- forM job_mvars takeMVar
  when (any not job_results) $ throwM $ DownloadException $ T.pack $
    "Failed to download " ++ show (length . filter not $ job_results) ++ " files"
