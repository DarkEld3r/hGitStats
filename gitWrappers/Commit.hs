{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Commit
  ( Commit
  , commitLookup
  , commitsLookup
  , commitFree
  , topologicalCommits
  , commitMessage
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Exception (assert)

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

-- /** Time in a signature */
--typedef struct git_time {
--	git_time_t time; /**< time in seconds from epoch */
--	int offset; /**< timezone offset, in minutes */
--} git_time;
--
-- /** An action signature (e.g. for committers, taggers, etc) */
--typedef struct git_signature {
--	char *name; /**< full name of the author */
--	char *email; /**< email of the author */
--	git_time when; /**< time when the action happened */
--} git_signature;

data CGitSignature = CGitSignature CInt CChar
  deriving (Show, Read, Eq)
type GitSignature = Ptr CGitSignature

instance Storable CGitSignature where
  sizeOf _ = 24
  alignment = sizeOf
  peek ptr = do
    name <- peekByteOff ptr 0
    email <- peekByteOff ptr 4
    time <- peekByteOff ptr 8
    return (CGitSignature name email)

foreign import ccall git_commit_committer :: Commit -> GitSignature

commiterName :: Commit -> IO String
commiterName commit = assert (commit /= nullPtr) $ do
  result <- git_commit_committer commit
  name $ peek result
