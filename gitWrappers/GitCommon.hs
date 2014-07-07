module GitCommon
  ( checkResult
  ) where

import Foreign.C.Types
import Control.Monad

-- Reports error if result is not zero.
checkResult :: IO CInt -> String -> IO ()
checkResult ioResult errorMessage = do
  result <- ioResult
  when (0 /= result) $ 
    error $ errorMessage ++ " Error code = '" ++ (show result) ++ "'."  
