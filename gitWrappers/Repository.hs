{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Repository
  ( Repository
  , repositoryOpen
  , repositoryFree
  , withRepositoryOpen
  , repositoryNamespace
  , repositoryPath
  , repositoryIsEmpty
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Control.Exception (assert, bracket)

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

foreign import ccall git_repository_get_namespace :: Repository -> IO CString

repositoryNamespace :: Repository -> IO String
repositoryNamespace repository = assert (repository /= nullPtr)
  git_repository_get_namespace repository >>= peekCString

foreign import ccall git_repository_path :: Repository -> IO CString

repositoryPath :: Repository -> IO String
repositoryPath repository = assert (repository /= nullPtr)
  git_repository_path repository >>= peekCString

foreign import ccall git_repository_is_empty :: Repository -> IO CInt

repositoryIsEmpty :: Repository -> IO Bool
repositoryIsEmpty repository = assert (repository /= nullPtr) $ do
  result <- git_repository_is_empty repository
  case result of
    1 -> return True
    0 -> return False
    _ -> error "Repository is corrupted."
