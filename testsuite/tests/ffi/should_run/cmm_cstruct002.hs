{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Prim
import GHC.Types
import GHC.Ptr
import Control.Exception
import Foreign
import Foreign.C

foreign import prim "cmm_test" test :: Int# -> (# Int#, Int#, Addr# #)

main :: IO ()
main = do
  let (# c, i1, p #) = test 1#
  assert (I# c == 3) (return ())
  assert (I# i1 == 2) (return ())
  i2 <- peek (Ptr p) :: IO CInt
  assert (i2 == 4) (return ())
