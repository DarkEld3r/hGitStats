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

--cmdParams :: Mode (CmdArgs CmdParams)
--cmdParams = cmdArgsMode $ CmdParams 
--  { path = def &= typFile &= help "Path to repository"
--  , count = def
--  }

cmdParams :: CmdParams
cmdParams = CmdParams 
  { path = def &= typFile &= help "Path to repository"
  , count = def
  }


-- TODO: FIXME:
-- bracket/finally/onException:

main :: IO ()
main = do
--  params <- cmdArgsRun cmdParams
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
