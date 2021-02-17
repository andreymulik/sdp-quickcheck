module Main where

import Prelude ()
import SDP.SafePrelude

import Criterion.Main ( env, bench, whnf, defaultMain )

import Test.QuickCheck
import Test.SDP.Gen

import SDP.Unrolled
import SDP.ByteList
import SDP.Array
import SDP.Bytes

import Control.DeepSeq.SDP as F ( force )

default ()

--------------------------------------------------------------------------------

main :: IO ()
main =  defaultMain
  [
    let arrayEnv        = F.force <$> generate (linearLargeA 10000) :: IO (Array Int Int)
    in  env arrayEnv    $ bench "bench-timsort-array" . whnf sort,
    
    let bytesEnv        = F.force <$> generate (linearLargeA 10000) :: IO (Bytes Int Int)
    in  env bytesEnv    $ bench "bench-timsort-bytes" . whnf sort,
    
    let unrolledEnv     = F.force <$> generate (linearLargeA 10000) :: IO (Unrolled Int Int)
    in  env unrolledEnv $ bench "bench-timsort-unrolled" . whnf sort,
    
    let bytelistEnv     = F.force <$> generate (linearLargeA 10000) :: IO (ByteList Int Int)
    in  env bytelistEnv $ bench "bench-timsort-bytelist" . whnf sort
  ]




