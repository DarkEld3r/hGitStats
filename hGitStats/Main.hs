import System.Environment
import GitWrappers

parseCommandLine :: IO String
parseCommandLine = do
  args <- getArgs
  case args of
    [] -> error "Error"
    _ -> return . head $ args

main :: IO ()
main = do
  path <- parseCommandLine
  putStrLn path
  gitRepositoryOpen 
