module Main where

import System.Environment

-- TODO: FIXME: Remove
import Control.Monad
import Foreign.Ptr

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
  when (headOid /= nullPtr) (putStrLn $ "test")
  repositoryFree repository
  return ()
