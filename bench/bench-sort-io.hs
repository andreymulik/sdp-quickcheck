module Main where

import Prelude ()
import SDP.SafePrelude
import SDP.Unrolled.IO
import SDP.ByteList.IO
import SDP.Array.IO
import SDP.Bytes.IO

import Criterion.Main ( bench, nfIO, defaultMain )

import Test.QuickCheck
import Test.SDP.Gen

import Control.DeepSeq.SDP as F ( force )

default ()

--------------------------------------------------------------------------------

main :: IO ()
main =  do
  list     <- F.force <$> generate (linearLargeA 100000) ::  IO [Int]
  array    <- newLinearN 100000 list :: IO (IOArray    Int Int)
  bytes    <- newLinearN 100000 list :: IO (IOBytes    Int Int)
  unrolled <- newLinearN 100000 list :: IO (IOUnrolled Int Int)
  bytelist <- newLinearN 100000 list :: IO (IOByteList Int Int)
  defaultMain
    [
      bench "bench-timsort-ioarray"    $ nfIO (sortM array >> sortedM array),
      bench "bench-timsort-iobytes"    $ nfIO (sortM bytes >> sortedM bytes),
      bench "bench-timsort-iounrolled" $ nfIO (sortM unrolled >> sortedM unrolled),
      bench "bench-timsort-iobytelist" $ nfIO (sortM bytelist >> sortedM bytelist)
    ]

