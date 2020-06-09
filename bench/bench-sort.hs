module Main where

import Prelude ()
import SDP.SafePrelude

import Criterion.Main ( bench, whnf, defaultMain )

import Test.QuickCheck
import Test.SDP.Gen

import SDP.Array
import SDP.Bytes

import SDP.Unrolled
import SDP.ByteList

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = do
  array    <- generate (linearA 10000) :: IO (Array    Int Int)
  bytes    <- generate (linearA 10000) :: IO (Bytes    Int Int)
  unrolled <- generate (linearA 10000) :: IO (Unrolled Int Int)
  bytelist <- generate (linearA 10000) :: IO (ByteList Int Int)
  
  defaultMain
    [
      bench "bench-timsort-array   " $ sort `whnf` array,
      bench "bench-timsort-bytes   " $ sort `whnf` bytes,
      bench "bench-timsort-unrolled" $ sort `whnf` unrolled,
      bench "bench-timsort-bytelist" $ sort `whnf` bytelist
    ]

