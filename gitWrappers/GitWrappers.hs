{-# LANGUAGE ForeignFunctionInterface #-}

module GitWrappers where

import Foreign.C
import Foreign.Ptr

-- git_repository
data CGitRepository = CGitRepository
-- git_repository *
type CGitRepositoryPtr = Ptr CGitRepository

foreign import ccall "git_repository_open" gitRepositoryOpen :: Ptr CGitRepository -> Ptr CChar -> CInt
