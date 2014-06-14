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

foreign import prim "cmm_test" test :: Int# -> (# Int#, Addr# #)

main :: IO ()
main = do
  let (# i, p #) = test 1#
  assert (I# i == 1) (return ())
  let ptr = Ptr p
  s1_c <- peek ptr :: IO CChar
  assert (s1_c == 2) (return ())
  s1_i <- peek (ptr `plusPtr` 4) :: IO CInt
  assert (s1_i == 3) (return ())
