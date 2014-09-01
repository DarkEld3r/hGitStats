{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Commit
  ( Commit
  , commitLookup
  , commitsLookup
  , commitFree
  , topologicalCommits
  , commitMessage
  , GitSignature
  , commiterName
  , commiterEmail
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

data CGitTime

instance Storable CGitTime where
  sizeOf _ = 16
  alignment = sizeOf

data CGitSignature = CGitSignature
  { signatureName :: CString
  , signatureEmail :: CString
  , signatureTime :: CGitTime
  }

instance Storable CGitSignature where
  sizeOf _ = 24
  alignment = sizeOf

type GitSignature = Ptr CGitSignature

foreign import ccall git_commit_committer :: Commit -> IO GitSignature

-- TODO: FIXME: withCommitCommiter
-- TODO: FIXME:
------ peekElemOff?
------ peekByteOff?

commiterName :: Commit -> IO String
commiterName commit = do
  result <- git_commit_committer commit >>= peek
  peekCString . signatureName $ result

commiterEmail :: Commit -> IO String
commiterEmail commit = do
  result <- git_commit_committer commit >>= peek
  peekCString . signatureEmail $ result

--commitTime :: Commit -> IO CGitTime
--commitTime commit = do
--  result <- git_commit_committer commit >>= peek
--  peekCString . signatureTime $ result


-- TODO: FIXME: git_signature_free
