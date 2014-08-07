{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Control.Monad (when)

import Repository
import Oid
import Revwalk
import Commit

data CmdParams = CmdParams
  { path :: FilePath
  , count :: Bool
  }
  deriving (Show, Data, Typeable)

cmdParams :: CmdParams
cmdParams = CmdParams 
  { path = def &= argPos 0 &= typ "<path to repository>"
  , count = def &= help "Show commits count"
  }

process :: Repository -> IO ()
process repository = do
  oids <- topologicalOids repository
  putStrLn ((++) "Total commitst count: " $ show . length $ oids)

main :: IO ()
main = do
  params <- cmdArgs cmdParams
{-
  withRepositoryOpen (path params) process
--    putStrLn ((++) "Total commitst count: " $ show . length $ oids)
-}



  repository <- repositoryOpen . path $ params
  oids <- topologicalOids repository

  when (count params) $
    putStrLn ((++) "Total commitst count: " $ show . length $ oids)


  commits <- commitsLookup repository oids
  messages <- mapM commitMessage commits

  print (take 5 messages)

  -- Free resources.  
  mapM_ commitFree commits
  mapM_ oidFree oids
  repositoryFree repository
