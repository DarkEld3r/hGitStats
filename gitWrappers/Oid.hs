{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Oid
  ( Oid
  , oidCreate
  , oidFree
  , withOid
  , oidFromStr
  , referenceNameToId
  , headId
  , withHeadId
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Control.Exception (assert)
import Control.Exception (bracket)

import Common
import Repository

-- git_oid
data CGitOid
type Oid = Ptr CGitOid

oidSize :: Int
oidSize = 20

oidCreate :: IO Oid
oidCreate = do
  mallocBytes oidSize

oidFree :: Oid -> IO ()
oidFree oid =
  free oid

withOid :: (Oid -> IO a) -> IO a
withOid = bracket oidCreate oidFree

foreign import ccall git_oid_fromstr :: Oid -> CString -> IO CInt

oidFromStr :: String -> IO Oid
oidFromStr hash = do
  oid <- oidCreate
  checkResult (withCString hash $ \hash' -> git_oid_fromstr oid hash') 
    $ "git_oid_fromstr(" ++ hash ++ ") failed."
  return oid

foreign import ccall git_reference_name_to_id :: Oid -> Repository -> CString -> IO CInt

referenceNameToId :: Repository -> String -> IO Oid
referenceNameToId repository name = assert (repository /= nullPtr) $ do
  oid <- oidCreate
  checkResult (withCString name $ \name' -> git_reference_name_to_id oid repository name') 
    $ "git_reference_name_to_id(" ++ name ++ ") failed."
  return oid

headId :: Repository -> IO Oid
headId repository = referenceNameToId repository "HEAD"

withHeadId :: Repository -> (Oid -> IO a) -> IO a
withHeadId repository = bracket (headId repository) oidFree
