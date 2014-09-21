{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Prim
import GHC.Types
import Control.Exception

foreign import prim "cmm_test" test :: Int# -> Int#

main :: IO ()
main = do
  let i1 = test 1#
  assert (I# i1 == 2) (return ())
