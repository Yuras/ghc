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

foreign import prim "cmm_test" test :: Int# -> (# Int#, Int#, Int# #)

main :: IO ()
main = do
  let (# c, i1, i2 #) = test 1#
  assert (I# c == 1) (return ())
  assert (I# i1 == 2) (return ())
  assert (I# i2 == 3) (return ())
