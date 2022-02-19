module MuchJMAP.Maildir where

import qualified Data.Aeson as Aeson
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import Data.List (isPrefixOf)

newtype Maildir = Maildir FilePath
  deriving (Show, Aeson.ToJSON, Aeson.FromJSON)


maildirTmpPath :: Maildir -> FilePath
maildirTmpPath (Maildir p) = p </> "tmp"

maildirNewPath :: Maildir -> FilePath
maildirNewPath (Maildir p) = p </> "new"

maildirCurPath :: Maildir -> FilePath
maildirCurPath (Maildir p) = p </> "cur"

maildirPath :: Maildir -> FilePath
maildirPath (Maildir p) = p

createMaildirIfMissing :: Maildir -> IO ()
createMaildirIfMissing maildir =
  createDirectoryIfMissing True (maildirTmpPath maildir) >>
  createDirectoryIfMissing True (maildirCurPath maildir) >>
  createDirectoryIfMissing True (maildirNewPath maildir)

listMails :: Maildir -> IO [FilePath]
listMails maildir =
  fmap
  (filter (\x -> not $ "." `isPrefixOf` takeFileName x)) $
  (++) <$>
   listDirectory (maildirNewPath maildir) <*>
   listDirectory (maildirCurPath maildir)
