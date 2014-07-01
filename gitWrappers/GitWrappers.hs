{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module GitWrappers
  ( GitRepository
  , git_repository_open
  , GitRevwalk
  , git_revwalk_new
  ) where

import Foreign.C
import Foreign.Ptr

-- git_repository
data CGitRepository
type GitRepository = Ptr CGitRepository

foreign import ccall git_repository_open :: Ptr GitRepository -> CString -> IO CInt

-- git_revwalk
data CGitRevwalk
type GitRevwalk = Ptr CGitRevwalk

foreign import ccall git_revwalk_new :: Ptr GitRevwalk -> GitRepository -> IO CInt
