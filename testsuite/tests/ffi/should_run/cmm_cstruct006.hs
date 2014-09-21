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

foreign import prim "cmm_test" test :: Int# -> (# Int#, Float# #)

main :: IO ()
main = do
  let (# c, f #) = test 1#
  assert (I# c == 3) (return ())
  let ff = F# f
  assert (ff > 1.99) (return ())
  assert (ff < 2.01) (return ())
