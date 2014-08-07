{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Repository
  ( Repository
  , repositoryOpen
  , repositoryFree
  , withRepositoryOpen
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Exception (bracket)

import Common

-- git_repository
data CGitRepository
type Repository = Ptr CGitRepository

foreign import ccall git_repository_open :: Ptr Repository -> CString -> IO CInt

repositoryOpen :: String -> IO Repository
repositoryOpen path = alloca $ \repository -> do
  checkResult (withCString path $ \path' -> git_repository_open repository path') 
    $ "Unable to open '" ++ path ++ "' repository."
  peek repository  

foreign import ccall git_repository_free :: Repository -> IO ()

repositoryFree :: Repository -> IO ()
repositoryFree repository = do
  git_repository_free repository

withRepositoryOpen :: String -> (Repository -> IO a) -> IO a
withRepositoryOpen path = bracket (repositoryOpen path) repositoryFree
