{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Revwalk
  ( Revwalk
  , revwalkNew
  , revwalkFree
  , withRevwalk
  , SortMode(..)
  , revwalkSorting
  , revwalkPush
  , revwalkNext
  , nextOids
  , topologicalOids
  ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Exception (assert, bracket)

import Common
import Repository
import Oid

-- git_revwalk
data CGitRevwalk
type Revwalk = Ptr CGitRevwalk

foreign import ccall git_revwalk_new :: Ptr Revwalk -> Repository -> IO CInt

revwalkNew :: Repository -> IO Revwalk
revwalkNew repository = alloca $ \revwalk -> assert (repository /= nullPtr) $ do
  checkResult (git_revwalk_new revwalk repository) "Unable to create revision walker."
  peek revwalk

foreign import ccall git_revwalk_free :: Revwalk -> IO ()

revwalkFree :: Revwalk -> IO ()
revwalkFree revwalk = do
  git_revwalk_free revwalk

withRevwalk :: Repository -> (Revwalk -> IO a) -> IO a
withRevwalk repository = bracket (revwalkNew repository) revwalkFree

foreign import ccall git_revwalk_sorting :: Revwalk -> CUInt -> IO ()

data SortMode = NoSort 
              | Topological
              | Time
              | TimeTopological
              | Reverse
              | ReverseTopological
              | ReverseTime
              | ReverseTimeTopological
  deriving (Enum)


revwalkSorting :: Revwalk -> SortMode -> IO ()
revwalkSorting revwalk sortMode = assert (revwalk /= nullPtr) $ do
  git_revwalk_sorting revwalk $ fromIntegral . fromEnum $ sortMode

foreign import ccall git_revwalk_push :: Revwalk -> Oid -> IO CInt

revwalkPush :: Revwalk -> Oid -> IO ()
revwalkPush revwalk oid = assert (revwalk /= nullPtr && oid /= nullPtr) $ do
 checkResult (git_revwalk_push revwalk oid) "git_revwalk_push failed."

foreign import ccall git_revwalk_next :: Oid -> Revwalk -> IO CInt

revwalkNext :: Revwalk -> IO (Maybe Oid)
revwalkNext revwalk = assert (revwalk /= nullPtr) $ do
  oid <- oidCreate
  result <- git_revwalk_next oid revwalk
  case result of
    0 -> return (Just oid)
    _ -> return Nothing

nextOids :: Revwalk -> IO [Oid]
nextOids revwalk = assert (revwalk /= nullPtr) $ do
  result <- revwalkNext revwalk
  case result of
    Just oid -> fmap ((:) oid) (nextOids revwalk)
    Nothing  -> return []

nextTopologicalOids :: Repository -> Oid -> IO [Oid]
nextTopologicalOids repository oid = assert (repository /= nullPtr && oid /= nullPtr) $ do
  withRevwalk repository $ \walker -> do
    revwalkSorting walker Topological
    revwalkPush walker oid
    nextOids walker

topologicalOids :: Repository -> IO [Oid]
topologicalOids repository = assert (repository /= nullPtr) $ do
  withHeadId repository (nextTopologicalOids repository)
