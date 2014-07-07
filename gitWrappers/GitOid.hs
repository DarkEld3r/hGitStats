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