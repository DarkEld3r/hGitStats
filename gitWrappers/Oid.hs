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

-- TODO: FIXME
import Foreign.Marshal.Alloc
import Foreign.Storable

import Common
import Repository

-- git_oid
-- TODO: Array (fixed size)?
oidRawSize = 20
data CGitOid = CGitOid { id :: CString }
type Oid = Ptr CGitOid

instance Storable CGitOid where
  alignment _ = (undefined :: Int)
  sizeOf _ = oidRawSize
  peek _ = error "peek is not implemented"
  poke ptr (CGitOid id') = do
    withCStringLen (take oidRawSize value) $ uncurry (copyArray $ #{ptrc_type, c_string_field} ptr)
--    (#poke ArrayStruct, a) ptr a'
--    (#poke ArrayStruct, b) ptr b'
--    (#poke ArrayStruct, c) ptr c'

foreign import ccall git_oid_fromstr :: Oid -> CString -> IO CInt

oidFromStr :: String -> IO Oid
oidFromStr hash = do
  -- TODO: FIXME: alloca?
  let oid = nullPtr
  checkResult (withCString hash $ \hash' -> git_oid_fromstr oid hash') 
    $ "git_oid_fromstr(" ++ hash ++ ") failed."
  return oid

foreign import ccall git_reference_name_to_id :: Oid -> Repository -> CString -> IO CInt

--referenceNameToId :: Repository -> String -> IO Oid
--referenceNameToId repository name = do
--  let oid = nullPtr
--  checkResult (withCString name $ \name' -> git_reference_name_to_id oid repository name') 
--    $ "git_reference_name_to_id(" ++ name ++ ") failed."
--  return oid

referenceNameToId :: Repository -> String -> IO Oid
referenceNameToId repository name = alloca $ \oid -> do
  checkResult (withCString name $ \name' -> git_reference_name_to_id oid repository name') 
    $ "git_reference_name_to_id(" ++ name ++ ") failed."
  return oid


headId :: Repository -> IO Oid
headId repository = referenceNameToId repository "HEAD"
