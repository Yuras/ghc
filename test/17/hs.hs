{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

import GHC.Prim
import GHC.Ptr
import GHC.Types
import Control.Exception
import Foreign
import Foreign.C

foreign import prim "cmm_test" test :: Int# -> (# Int#, Float#, Double# #)

main :: IO ()
main = do
  let (# i, f, d #) = test 1#
      ff = F# f
      dd = D# d
  assert (ff > 1.99 && ff < 2.1) (return ())
  assert (dd > 2.99 && dd < 3.1) (return ())
