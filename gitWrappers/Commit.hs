{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Commit
  ( Commit
  , commitLookup
  , commitsLookup
  , commitFree
  , topologicalCommits
  , commitMessage
  , committerName
  , committerEmail
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Exception (assert, bracket)

import Common
import Repository
import Oid
import Revwalk

-- git_oid
data CGitCommit
type Commit = Ptr CGitCommit

foreign import ccall git_commit_lookup :: Ptr Commit -> Repository -> Oid -> IO CInt

commitLookup :: Repository -> Oid -> IO Commit
commitLookup repository oid = alloca $ \commit -> assert (repository /= nullPtr && oid /= nullPtr) $ do
  checkResult (git_commit_lookup commit repository oid)
    $ "git_commit_lookup failed."
  peek commit

commitsLookup :: Repository -> [Oid] -> IO [Commit]
commitsLookup repository oids = assert (repository /= nullPtr)
  mapM (commitLookup repository) oids  

foreign import ccall git_commit_free :: Commit -> IO ()

commitFree :: Commit -> IO ()
commitFree commit = do
  git_commit_free commit

topologicalCommits :: Repository -> IO [Commit]
topologicalCommits repository = assert (repository /= nullPtr) $ do
  withTopologicalOids repository $ \oids -> do
    commitsLookup repository oids

foreign import ccall git_commit_message :: Commit -> IO CString

commitMessage :: Commit -> IO String
commitMessage commit = assert (commit /= nullPtr) $ do
  result <- git_commit_message commit
  peekCString result

data CGitSignature

instance Storable CGitSignature where
  sizeOf _ = 24
  alignment = sizeOf

foreign import ccall git_commit_committer :: Commit -> IO (Ptr CGitSignature)

foreign import ccall git_signature_free :: Ptr CGitSignature -> IO ()

withCommitCommitter :: Commit -> (Ptr CGitSignature -> IO a) -> IO a
withCommitCommitter commit = bracket (git_commit_committer commit) git_signature_free

readCommitterString :: Ptr CGitSignature -> Int -> IO String
readCommitterString committer offset = assert (committer /= nullPtr) $ do
  cName <- peekByteOff committer offset :: IO CString
  peekCString cName

nameOffset :: Int
nameOffset = 0

committerName :: Commit -> IO String
committerName commit = assert (commit /= nullPtr) $ do
  withCommitCommitter commit $ \committer -> do
    readCommitterString committer nameOffset

emailOffset :: Int
emailOffset = 4

committerEmail :: Commit -> IO String
-- assert (commit /= nullPtr)
committerEmail commit = assert (commit /= nullPtr) $ do
  signature <- git_commit_committer commit
  cName <- peekByteOff signature emailOffset :: IO CString
  peekCString cName
