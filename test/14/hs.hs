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

foreign import prim "cmm_test" test :: Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> (# Int#, Int#, Int#, Int#, Int#, Int# #)

main :: IO ()
main = do
  let (# ii1, ii2, ii3, ii4, ii5, ii6 #) = test 1# 2# 3# 4# 5# 6# 7#
  assert (I# ii1 == 1) (return ())
  assert (I# ii2 == 2) (return ())
  assert (I# ii3 == 3) (return ())
  assert (I# ii4 == 4) (return ())
  assert (I# ii5 == 5) (return ())
  assert (I# ii6 == 6) (return ())
