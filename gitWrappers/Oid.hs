{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Oid
  ( Oid
  , createOid
  , freeOid
  , oidFromStr
  , referenceNameToId
  , headId
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Common
import Repository

-- git_oid
data CGitOid
type Oid = Ptr CGitOid

oidSize :: Int
oidSize = 20

createOid :: IO Oid
createOid = do
  mallocBytes oidSize

freeOid :: Oid -> IO ()
freeOid oid = do
  free oid

foreign import ccall git_oid_fromstr :: Oid -> CString -> IO CInt

oidFromStr :: String -> IO Oid
oidFromStr hash = do
  oid <- createOid
  checkResult (withCString hash $ \hash' -> git_oid_fromstr oid hash') 
    $ "git_oid_fromstr(" ++ hash ++ ") failed."
  return oid

foreign import ccall git_reference_name_to_id :: Oid -> Repository -> CString -> IO CInt

referenceNameToId :: Repository -> String -> IO Oid
referenceNameToId repository name = do
  oid <- createOid
  -- TODO: FIXME
  print . show $ oid
  print . show $ repository
  checkResult (withCString name $ \name' -> git_reference_name_to_id oid repository name') 
    $ "git_reference_name_to_id(" ++ name ++ ") failed."
  -- TODO: FIXME
  print . show $ oid
  return oid

headId :: Repository -> IO Oid
headId repository = referenceNameToId repository "HEAD"
