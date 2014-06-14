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

foreign import prim "cmm_test" test :: Int# -> Int# -> Int# -> Int# -> Int# -> Int#

main :: IO ()
main = do
  let ii1 = test 1# 2# 3# 4# 5#
  assert (I# ii1 == 1) (return ())
