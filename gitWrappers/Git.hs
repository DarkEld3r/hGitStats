module Git
  ( gitRepositoryOpen
  , gitRevwalkNew
  ) where

import Control.Monad
import Foreign.Ptr
import Foreign.C.String

import GitWrappers

gitRepositoryOpen :: String -> IO GitRepository
gitRepositoryOpen path = do
  let repository = nullPtr
  result <- withCString path $ \path' -> git_repository_open repository path'
  when (0 /= result) $ 
    error $ "Unable to open '" ++ path ++ "' repository. Error = '" ++ (show result) ++ "'."
  return repository

gitRevwalkNew :: GitRepository -> IO GitRevwalk
gitRevwalkNew repository = do
  let revwalk = nullPtr
  result <- git_revwalk_new revwalk repository 
  when (0 /= result) $ 
    error $ "Unable to create revision walker. Error = '" ++ (show result) ++ "'."
  return revwalk
