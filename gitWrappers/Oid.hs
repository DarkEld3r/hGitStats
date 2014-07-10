{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Oid
  ( Oid
  , oidFromStr
  , referenceNameToId
  , headId
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Common

-- git_oid
data CGitOid
type Oid = Ptr CGitOid

foreign import ccall git_oid_fromstr :: Oid -> CString -> IO CInt

oidFromStr :: String -> IO GitOid
oidFromStr hash = do
  -- TODO: FIXME: alloca?
  let oid = nullPtr
  checkResult (withCString hash $ \hash' -> git_oid_fromstr oid hash') 
    $ "git_oid_fromstr(" ++ hash ++ ") failed."
  return oid

foreign import ccall git_reference_name_to_id :: Oid -> GitRepository -> CString -> IO CInt

referenceNameToId :: GitRepository -> String -> IO Oid
referenceNameToId repository name = do
  -- TODO: FIXME: alloca?
  let oid = nullPtr
  checkResult (withCString name $ \name' -> git_reference_name_to_id oid repository name') 
    $ "git_reference_name_to_id(" ++ name ++ ") failed."
  return oid

headId :: GitRepository -> IO Oid
headId repository = referenceNameToId repository "HEAD"
