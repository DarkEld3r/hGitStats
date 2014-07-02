{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module GitWrappers
  ( GitRepository
  , git_repository_open
  , git_repository_free
  , GitRevwalk
  , git_revwalk_new
  , git_revwalk_free
  ) where

import Foreign.C
import Foreign.Ptr

-- git_repository
data CGitRepository
type GitRepository = Ptr CGitRepository

foreign import ccall git_repository_open :: Ptr GitRepository -> CString -> IO CInt
foreign import ccall git_repository_free :: GitRepository -> IO ()

-- git_revwalk
data CGitRevwalk
type GitRevwalk = Ptr CGitRevwalk

foreign import ccall git_revwalk_new :: Ptr GitRevwalk -> GitRepository -> IO CInt
foreign import ccall git_revwalk_free :: GitRevwalk -> IO CInt

-- TODO: FIXME: free functions

-- TODO: FIXME:
-- git_commit_message
-- git_commit_committer