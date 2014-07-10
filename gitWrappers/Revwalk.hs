{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Revwalk
  ( Revwalk
  , gitRevwalkNew
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Common
import Oid

-- git_revwalk
data CGitRevwalk
type Revwalk = Ptr CGitRevwalk

foreign import ccall git_revwalk_new :: Ptr Revwalk -> GitRepository -> IO CInt

gitRevwalkNew :: GitRepository -> IO Revwalk
gitRevwalkNew repository = do
  let revwalk = nullPtr
  checkResult (git_revwalk_new revwalk repository) "Unable to create revision walker."
  return revwalk

foreign import ccall git_revwalk_free :: Revwalk -> IO ()

gitRevwalkFree :: GitRevwalk -> IO ()
gitRevwalkFree revwalk = do
  git_revwalk_free revwalk

foreign import ccall git_revwalk_next :: GitOid -> Revwalk -> IO CInt
foreign import ccall git_revwalk_sorting :: Revwalk -> CUInt -> IO ()
foreign import ccall git_revwalk_push :: Revwalk -> GitOid -> IO CInt
