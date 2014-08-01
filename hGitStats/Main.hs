module Main where

import System.Environment (getArgs)

import Repository
import Oid
import Revwalk
import Commit

parseCommandLine :: IO String
parseCommandLine = do
  args <- getArgs
  case args of
    [] -> error "Specify path to repository."
    _ -> return . head $ args

-- TODO: FIXME:
-- bracket/finally/onException:

main :: IO ()
main = do
  path <- parseCommandLine
  repository <- repositoryOpen path

  oids <- topologicalOids repository
  commits <- mapM (commitLookup repository) oids
  messages <- mapM commitMessage commits

  print (take 5 messages)

  -- Free resources.  
  mapM_ commitFree commits
  mapM_ oidFree oids
  repositoryFree repository
