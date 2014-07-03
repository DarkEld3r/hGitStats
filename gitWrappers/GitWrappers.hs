{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module GitWrappers
  ( GitRepository
  , git_repository_open
  , git_repository_free
  , GitRevwalk
  , git_revwalk_new
  , git_revwalk_free
  , git_revwalk_sorting
  , GitOid
  , git_revwalk_push
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
foreign import ccall git_revwalk_free :: GitRevwalk -> IO ()

--newtype WalkSorting = WalkSorting { sorting :: CInt }
-- #{enum WalkSorting, WalkSorting,
--  none        = 0,
--  topological = shiftL 1 0,
--  time        = shiftL 1 1,
--  reverse     = shiftL 1 2,
--}

foreign import ccall git_revwalk_sorting :: GitRevwalk -> CUInt -> IO ()

data CGitOid
type GitOid = Ptr CGitOid

-- TODO: const pointer?
foreign import ccall git_revwalk_push :: GitRevwalk -> GitOid -> IO CInt

-- git_revwalk_sorting

-- TODO: FIXME: free functions

-- TODO: FIXME:
-- git_commit_message
-- git_commit_committer