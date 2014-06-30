module Git (gitRepositoryOpen) where

import Control.Monad
import Foreign.Ptr
import Foreign.C.String

import GitWrappers

gitRepositoryOpen :: String -> IO GitRepository
gitRepositoryOpen path = do
  let repository = nullPtr
  result <- withCString path $ \path' -> git_repository_open repository path'
  when (0 /= result) $ error $ "Unable to open '" ++ path ++ "' repository."
  return repository
