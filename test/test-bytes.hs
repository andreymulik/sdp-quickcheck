module Main where

import Test.Framework.Providers.QuickCheck2
import Test.Framework

import SDP.Bytes

import Test.SDP.Arbitrary ()
import Test.SDP

default ()

--------------------------------------------------------------------------------

main :: IO ()
main =  defaultMain
  [
    -- Eq tests
    testProperty "bytes-eq-consistency   " eqConsistencyProp,
    testProperty "bytes-eq-transitive    " eqTransitiveProp,
    testProperty "bytes-eq-symmetric     " eqSymmetricProp,
    testProperty "bytes-eq-reflexive     " eqReflexiveProp,
    
    -- Ord tests
    testProperty "bytes-ord-antisymmetry " ordAntisymmetryProp,
    testProperty "bytes-ord-transitive   " ordTransitiveProp,
    testProperty "bytes-ord-totality     " ordTotalityProp,
    testProperty "bytes-lexicographic    " lexicographicOrdProp,
    
    -- linear tests
    testProperty "bytes-linear-basic     " basicLinearProp,
    testProperty "bytes-linear-decons    " deconstructionLinearProp,
    testProperty "bytes-linear-cons      " constructionLinearProp,
    testProperty "bytes-linear-reverse   " reverseProp,
    testProperty "bytes-linear-concat    " concatProp,
    
    -- split test
    testProperty "bytes-split            " splitProp,
    
    -- indexed tests
    testProperty "bytes-indexed-basic    " basicIndexedProp,
    testProperty "bytes-indexed-assoc    " assocIndexedProp,
    testProperty "bytes-indexed-read     " readIndexedProp,
    
    -- sort test
    testProperty "bytes-sort             " sortProp,
    
    -- set test
    testProperty "bytes-set              " setProp,
    
    -- estimate test
    testProperty "bytes-estimate         " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqConsistencyProp :: Bytes Int Int -> Bytes Int Int -> Bool
eqConsistencyProp =  eqConsistencyTest

eqTransitiveProp :: Bytes Int Int -> Bytes Int Int -> Bytes Int Int -> Bool
eqTransitiveProp =  eqTransitiveTest

eqSymmetricProp :: Bytes Int Int -> Bytes Int Int -> Bool
eqSymmetricProp =  eqSymmetricTest

eqReflexiveProp :: Bytes Int Int -> Bool
eqReflexiveProp =  eqReflexiveTest

--------------------------------------------------------------------------------

{- Ord property. -}

ordAntisymmetryProp :: Bytes Int Int -> Bytes Int Int -> Bool
ordAntisymmetryProp =  ordAntisymmetryTest

ordTransitiveProp :: Bytes Int Int -> Bytes Int Int -> Bytes Int Int -> Bool
ordTransitiveProp =  ordTransitiveTest

ordTotalityProp :: Bytes Int Int -> Bytes Int Int -> Bool
ordTotalityProp =  ordTotalityTest

lexicographicOrdProp :: Long (Bytes Int Int) -> Long (Bytes Int Int) -> Bool
lexicographicOrdProp (Long xs) (Long ys) = lexicographicOrdTest xs ys

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp :: Char -> Bytes Int Char -> Bool
basicLinearProp =  basicLinearTest

deconstructionLinearProp :: Bytes Int Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp :: Char -> Bytes Int Char -> Bool
constructionLinearProp =  constructionLinearTest

reverseProp :: Bytes Int Char -> Bool
reverseProp =  reverseTest

replicateProp :: TestLinear2 Bytes Int Char
replicateProp =  replicateTest

concatProp :: Bytes Int Char -> Bool
concatProp =  concatTest

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


