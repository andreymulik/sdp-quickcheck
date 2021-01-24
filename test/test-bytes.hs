module Main where

import Test.Framework.Providers.QuickCheck2
import Test.Framework

import SDP.Bytes

import Test.SDP.Arbitrary ()
import Test.SDP

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- common tests
    testProperty "bytes-eq             " eqProp,
    testProperty "bytes-ord            " ordProp,
    testProperty "bytes-lexicographic  " lgoProp,
    
    -- linear tests
    testProperty "bytes-linear-basic   " basicLinearProp,
    testProperty "bytes-linear-decons  " deconstructionLinearProp,
    testProperty "bytes-linear-cons    " constructionLinearProp,
    testProperty "bytes-linear-reverse " reverseProp,
    testProperty "bytes-linear-concat  " concatProp,
    
    -- split test
    testProperty "bytes-split          " splitProp,
    
    -- indexed tests
    testProperty "bytes-indexed-basic  " basicIndexedProp,
    testProperty "bytes-indexed-assoc  " assocIndexedProp,
    testProperty "bytes-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "bytes-sort           " sortProp,
    
    -- set test
    testProperty "bytes-set            " setProp,
    
    -- estimate test
    testProperty "bytes-estimate       " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqProp :: TestEq (Bytes Int Int)
eqProp =  eqTest

--------------------------------------------------------------------------------

{- Ord property. -}

ordProp :: TestOrd (Bytes Int Int)
ordProp =  ordTest

lgoProp :: Long (Bytes Int Int) -> Long (Bytes Int Int) -> Bool
lgoProp (Long xs) (Long ys) = lexicographicOrdTest xs ys

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp          :: Char -> Bytes Int Char -> Bool
basicLinearProp          =  basicLinearTest

deconstructionLinearProp :: Bytes Int Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp   :: Char -> Bytes Int Char -> Bool
constructionLinearProp   =  constructionLinearTest

reverseProp              :: Bytes Int Char -> Bool
reverseProp              =  reverseTest

replicateProp            :: TestLinear2 Bytes Int Char
replicateProp            =  replicateTest

concatProp               :: Bytes Int Char -> Bool
concatProp               =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: Char -> TestSplit2 Bytes Int Char
splitProp =  splitTest . (>)

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed2 Bytes Int Char
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed2 Bytes Int Char
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed2 Bytes Int Char
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Medium (Bytes Int Char) -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Set property. -}

setProp :: TestSet1 (Bytes Int) Char
setProp =  setTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate (Bytes Int Int)
estimateProp =  estimateTest




