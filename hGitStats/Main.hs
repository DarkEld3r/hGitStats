module Main where

import System.Environment
import Git

parseCommandLine :: IO String
parseCommandLine = do
  args <- getArgs
  case args of
    [] -> error "Error"
    _ -> return . head $ args

main :: IO ()
main = do
  path <- parseCommandLine
  repository <- gitRepositoryOpen path
  putStrLn "OK"
