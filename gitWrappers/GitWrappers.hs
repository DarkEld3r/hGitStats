{-# LANGUAGE ForeignFunctionInterface #-}

--module GitWrappers (GitRepository, git_repository_open) where
module GitWrappers where

import Foreign.C
import Foreign.Ptr

-- git_repository
data CGitRepository = CGitRepository
type GitRepository = Ptr CGitRepository

foreign import ccall git_repository_open :: Ptr GitRepository -> CString -> IO CInt
