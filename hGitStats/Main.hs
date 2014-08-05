{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

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


-- TODO: FIXME:
-- bracket/finally/onException:

main :: IO ()
main = do
  params <- cmdArgs cmdParams
  repository <- repositoryOpen . path $ params

  oids <- topologicalOids repository
  commits <- commitsLookup repository oids
  messages <- mapM commitMessage commits

  print (take 5 messages)

  -- Free resources.  
  mapM_ commitFree commits
  mapM_ oidFree oids
  repositoryFree repository
