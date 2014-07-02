module Git
  ( gitRepositoryOpen
  , gitRevwalkNew
  ) where

import Control.Monad
import Foreign.Ptr
import Foreign.C

import GitWrappers

-- Reports error if result is not zero.
checkResult :: IO CInt -> String -> IO ()
checkResult ioResult errorMessage = do
  result <- ioResult
  when (0 /= result) $ 
    error $ errorMessage ++ " Error code = '" ++ (show result) ++ "'."  
  return ()

gitRepositoryOpen :: String -> IO GitRepository
gitRepositoryOpen path = do
  let repository = nullPtr
  checkResult (withCString path $ \path' -> git_repository_open repository path') 
    $ "Unable to open '" ++ path ++ "' repository."
  return repository

gitRevwalkNew :: GitRepository -> IO GitRevwalk
gitRevwalkNew repository = do
  let revwalk = nullPtr
  checkResult (git_revwalk_new revwalk repository) "Unable to create revision walker."
  return revwalk
