{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module GitWrappers
  ( GitRepository
  , git_repository_open
  , git_repository_free
  , GitOid
  , git_oid_fromstr
  , GitRevwalk
  , git_revwalk_new
  , git_revwalk_free
  , git_revwalk_sorting
  , git_revwalk_next
  , git_revwalk_push
  ) where

import Foreign.C
import Foreign.Ptr

-- git_repository
data CGitRepository
type GitRepository = Ptr CGitRepository

foreign import ccall git_repository_open :: Ptr GitRepository -> CString -> IO CInt
foreign import ccall git_repository_free :: GitRepository -> IO ()

-- git_oid
data CGitOid
type GitOid = Ptr CGitOid

foreign import ccall git_oid_fromstr :: GitOid -> CString -> IO CInt

-- git_revwalk
data CGitRevwalk
type GitRevwalk = Ptr CGitRevwalk

foreign import ccall git_revwalk_new :: Ptr GitRevwalk -> GitRepository -> IO CInt
foreign import ccall git_revwalk_free :: GitRevwalk -> IO ()
foreign import ccall git_revwalk_next :: GitOid -> GitRevwalk -> IO CInt
foreign import ccall git_revwalk_sorting :: GitRevwalk -> CUInt -> IO ()

-- TODO: const pointer?
foreign import ccall git_revwalk_push :: GitRevwalk -> GitOid -> IO CInt

-- git_revwalk_sorting

-- TODO: FIXME: free functions

-- TODO: FIXME:
-- git_commit_message
-- git_commit_committer