{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module GitRevwalk
  ( GitRevwalk
  , gitOidFromStr
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import GitCommon
import GitOid

-- git_revwalk
data CGitRevwalk
type GitRevwalk = Ptr CGitRevwalk

foreign import ccall git_revwalk_new :: Ptr GitRevwalk -> GitRepository -> IO CInt

gitRevwalkNew :: GitRepository -> IO GitRevwalk
gitRevwalkNew repository = do
  let revwalk = nullPtr
  checkResult (git_revwalk_new revwalk repository) "Unable to create revision walker."
  return revwalk

foreign import ccall git_revwalk_free :: GitRevwalk -> IO ()

gitRevwalkFree :: GitRevwalk -> IO ()
gitRevwalkFree revwalk = do
  git_revwalk_free revwalk

foreign import ccall git_revwalk_next :: GitOid -> GitRevwalk -> IO CInt
foreign import ccall git_revwalk_sorting :: GitRevwalk -> CUInt -> IO ()
foreign import ccall git_revwalk_push :: GitRevwalk -> GitOid -> IO CInt
