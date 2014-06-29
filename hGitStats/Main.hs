import GitWrappers

main :: IO ()
main = do
  putStrLn . show . f1 $ 5
