module Main where

import System.Environment

import Repository
import Oid

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
  headOid <- headId repository
  freeOid headOid
  repositoryFree repository
  return ()
