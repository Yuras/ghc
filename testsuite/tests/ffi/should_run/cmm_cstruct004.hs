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

foreign import prim "cmm_test" test :: Int# -> (# Int#, Float#, Double# #)

main :: IO ()
main = do
  let (# c, f, d #) = test 1#
  assert (I# c == 1) (return ())
  let ff = F# f
      dd = D# d
  assert (ff > 1.39 && ff < 1.41) (return ())
  assert (dd > 1.49 && dd < 1.51) (return ())
