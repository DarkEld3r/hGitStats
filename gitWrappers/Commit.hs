{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Commit
  (
  ) where

--import Foreign.C.Types
--import Foreign.C.String
--import Foreign.Ptr
--import Foreign.Marshal.Alloc
--import Control.Exception (assert)

import Common
import Repository
import Oid

-- git_oid
data CGitCommit
type Commit = Ptr CGitCommit

foreign import ccall git_commit_lookup :: Ptr Commit -> Repository -> Oid -> IO CInt

commitlookup :: Repository -> Oid -> IO Commit
commitlookup repository oid = do
