{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module GitOid
  ( GitOid
  , gitOidFromStr
  ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import GitCommon

-- git_oid
data CGitOid
type GitOid = Ptr CGitOid

foreign import ccall git_oid_fromstr :: GitOid -> CString -> IO CInt

gitOidFromStr :: String -> IO GitOid
gitOidFromStr hash = do
  -- TODO: FIXME: alloca?
  let oid = nullPtr
  checkResult (withCString hash $ \hash' -> git_oid_fromstr oid hash') 
    $ "git_oid_fromstr(" ++ hash ++ ") failed."
  return oid

foreign import ccall git_reference_name_to_id :: GitOid -> GitRepository -> CString -> IO CInt

gitReferenceNameToId :: GitRepository -> String -> IO GitOid
gitReferenceNameToId repository name = do
  -- TODO: FIXME: alloca?
  let oid = nullPtr
  checkResult (withCString name $ \name' -> git_reference_name_to_id oid repository name') 
    $ "git_reference_name_to_id(" ++ name ++ ") failed."
  return oid

gitHeadId :: GitRepository -> IO GitOid
gitHeadId repository = gitReferenceNameToId repository "HEAD"
