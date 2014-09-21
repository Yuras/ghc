{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Prim
import GHC.Types
import Control.Exception

foreign import prim "cmm_test" test
  :: Int# -> (# Int#, Int#, Int#, Int#, Int# #)

main :: IO ()
main = do
  let (# i1, i2, i3, i4, i5 #) = test 1#
  assert (I# i1 == 1) (return ())
  assert (I# i2 == 2) (return ())
  assert (I# i3 == 3) (return ())
  assert (I# i4 == 4) (return ())
  assert (I# i5 == 5) (return ())
