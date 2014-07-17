{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Revwalk
  ( Revwalk
  , gitRevwalkNew
  , gitRevwalkFree
  ) where

import Foreign.C.Types
--import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Exception (assert)

import Common
import Repository
--import Oid

-- git_revwalk
data CGitRevwalk
type Revwalk = Ptr CGitRevwalk

foreign import ccall git_revwalk_new :: Ptr Revwalk -> Repository -> IO CInt

gitRevwalkNew :: Repository -> IO Revwalk
gitRevwalkNew repository = alloca $ \revwalk -> assert (repository /= nullPtr) $ do
  checkResult (git_revwalk_new revwalk repository) "Unable to create revision walker."
  peek revwalk

foreign import ccall git_revwalk_free :: Revwalk -> IO ()

gitRevwalkFree :: Revwalk -> IO ()
gitRevwalkFree revwalk = do
  git_revwalk_free revwalk

-- TODO: FIXME.
--foreign import ccall git_revwalk_next :: Oid -> Revwalk -> IO CInt
--foreign import ccall git_revwalk_sorting :: Revwalk -> CUInt -> IO ()
--foreign import ccall git_revwalk_push :: Revwalk -> Oid -> IO CInt
