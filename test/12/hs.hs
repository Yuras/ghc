{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

import GHC.Prim
import GHC.Types
import Control.Exception

foreign import prim "cmm_test" test :: Int# -> (# Int#, Int#, Float#, Float# #)

main :: IO ()
main = do
  let (# i1, i2, f1, f2 #) = test 1#
  assert (I# i1 == 1) (return ())
  assert (I# i2 == 2) (return ())
  let f1_ = F# f1
  assert (f1_ > 3.29 && f1_ < 3.31) (return ())
  let f2_ = F# f2
  assert (f2_ > 4.39 && f1_ < 4.41) (return ())
