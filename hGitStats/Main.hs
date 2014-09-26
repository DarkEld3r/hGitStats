{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Control.Monad (when)
import qualified Data.HashMap as HM

import Repository
import Commit

data CmdParams = CmdParams
  { path :: FilePath
  , count :: Bool
  , messages :: Bool 
  , statistics :: Bool
  }
  deriving (Show, Data, Typeable)

cmdParams :: CmdParams
cmdParams = CmdParams 
  { path = def &= argPos 0 &= typ "<path to repository>"
  , count = def &= help "Show commits count"
  , messages = def &= help "Show commits messages"
  , statistics = def &= help "Show committers statistics"
  }

delimiter :: String
delimiter = "-----------------------------------------"

printGeneralInformation :: Repository -> IO ()
printGeneralInformation repository = do
  putStrLn delimiter
  putStr "Repository path: "
  repositoryPath repository >>= putStrLn
  putStr "Repository namespace: "  
  repositoryNamespace repository >>= putStrLn
  putStr "Empty repository: "  
  repositoryIsEmpty repository >>= print

printCommitsCount :: [Commit] -> IO ()
printCommitsCount commits = do
  putStrLn delimiter
  putStrLn ((++) "Total commits count: " $ show . length $ commits)  

printMessages :: [Commit] -> IO ()
printMessages commits = do
  putStrLn delimiter
  mapM commitMessage commits >>= mapM_ print

commitsAuthors :: [Commit] -> IO [String]
commitsAuthors commits = mapM committerName commits

incrementCommitsCount :: Int -> Int -> Int
incrementCommitsCount _ value = 1 + value

updateMap :: String -> HM.Map String Int -> HM.Map String Int
updateMap author commitsMap = HM.insertWith incrementCommitsCount author 1 commitsMap

processAuthors :: [String] -> HM.Map String Int -> HM.Map String Int
processAuthors [] commitsMap = commitsMap
processAuthors [x] commitsMap = updateMap x commitsMap
processAuthors (x:xs) commitsMap = processAuthors xs (updateMap x commitsMap)

printStatistics :: [Commit] -> IO ()
printStatistics commits = do
  putStrLn delimiter
  authors <- commitsAuthors commits
--    let commitsMap = HM.empty
--    print commitsMap
  -- TODO: FIXME:
  print commits

main :: IO ()
main = do
  params <- cmdArgs cmdParams
  repository <- repositoryOpen . path $ params
  printGeneralInformation repository

  -- We need commits for many operations.
  commits <- topologicalCommits repository

  -- Process parameters.
  when (count params) $ printCommitsCount commits
  when (messages params) $ printMessages commits
  when (statistics params) $ printStatistics commits

  -- Free resources.  
  mapM_ commitFree commits
  repositoryFree repository
