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

delimiter :: String
delimiter = "-----------------------------------------"

printGeneralInformation :: Repository -> IO ()
printGeneralInformation repository = do
  putStrLn delimiter
  print repository
  namespace <- repositoryNamespace repository
  print namespace
--  putStrLn $ "Repository namespace: " ++ namespace

printCommitsCount :: [Oid] -> IO ()
printCommitsCount oids = do
  putStrLn delimiter
  putStrLn ((++) "Total commitst count: " $ show . length $ oids)  

main :: IO ()
main = do
  params <- cmdArgs cmdParams
  repository <- repositoryOpen . path $ params

  -- General repository information.
  printGeneralInformation repository

  oids <- topologicalOids repository

  -- Commits count.
  when (count params) $ printCommitsCount oids


  commits <- commitsLookup repository oids
  messages <- mapM commitMessage commits

  print (take 5 messages)

  -- Free resources.  
  mapM_ commitFree commits
  mapM_ oidFree oids
  repositoryFree repository
