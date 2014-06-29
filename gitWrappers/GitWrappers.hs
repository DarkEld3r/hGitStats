{-# LANGUAGE ForeignFunctionInterface #-}

module GitWrappers where

import Foreign.C

--foreign import ccall "repository.h" git_repository_open :: CDouble -> CDouble

foreign import ccall f1 :: CInt -> CInt
