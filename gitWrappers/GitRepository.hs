{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module GitRepository
  ( GitRepository
  , gitRepositoryOpen
  , gitRepositoryFree
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import GitCommon

-- git_repository
data CGitRepository
type GitRepository = Ptr CGitRepository

foreign import ccall git_repository_open :: Ptr GitRepository -> CString -> IO CInt

gitRepositoryOpen :: String -> IO GitRepository
gitRepositoryOpen path = do
  let repository = nullPtr
  checkResult (withCString path $ \path' -> git_repository_open repository path') 
    $ "Unable to open '" ++ path ++ "' repository."
  return repository

foreign import ccall git_repository_free :: GitRepository -> IO ()

gitRepositoryFree :: GitRepository -> IO ()
gitRepositoryFree repository = do
  git_repository_free repository
