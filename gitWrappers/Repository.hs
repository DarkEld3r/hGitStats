{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Repository
  ( Repository
  , repositoryOpen
  , repositoryFree
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Common

-- git_repository
data CGitRepository
type Repository = Ptr CGitRepository

foreign import ccall git_repository_open :: Ptr Repository -> CString -> IO CInt

repositoryOpen :: String -> IO Repository
repositoryOpen path = do
  let repository = nullPtr
  checkResult (withCString path $ \path' -> git_repository_open repository path') 
    $ "Unable to open '" ++ path ++ "' repository."
  return repository

foreign import ccall git_repository_free :: Repository -> IO ()

repositoryFree :: Repository -> IO ()
repositoryFree repository = do
  git_repository_free repository
