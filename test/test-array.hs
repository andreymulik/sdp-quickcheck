module Main where

import Test.Framework.Providers.QuickCheck2
import Test.Framework

import SDP.Array

import Test.SDP.Arbitrary ()
import Test.SDP

default ()

--------------------------------------------------------------------------------

main :: IO ()
main =  defaultMain
  [
    -- Eq tests
    testProperty "array-eq-consistency   " eqConsistencyProp,
    testProperty "array-eq-transitive    " eqTransitiveProp,
    testProperty "array-eq-symmetric     " eqSymmetricProp,
    testProperty "array-eq-reflexive     " eqReflexiveProp,
    
    -- Ord tests
    testProperty "array-ord-antisymmetry " ordAntisymmetryProp,
    testProperty "array-ord-transitive   " ordTransitiveProp,
    testProperty "array-ord-totality     " ordTotalityProp,
    testProperty "array-lexicographic    " lexicographicOrdProp,
    
    -- linear tests
    testProperty "array-linear-basic     " basicLinearProp,
    testProperty "array-linear-decons    " deconstructionLinearProp,
    testProperty "array-linear-cons      " constructionLinearProp,
    testProperty "array-linear-reverse   " reverseProp,
    testProperty "array-linear-concat    " concatProp,
    
    -- split test
    testProperty "array-split            " splitProp,
    
    -- indexed tests
    testProperty "array-indexed-basic    " basicIndexedProp,
    testProperty "array-indexed-assoc    " assocIndexedProp,
    testProperty "array-indexed-read     " readIndexedProp,
    
    -- sort test
    testProperty "array-sort             " sortProp,
    
    -- set test
    testProperty "array-set              " setProp,
    
    -- estimate test
    testProperty "array-estimate         " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqConsistencyProp :: Array Int Int -> Array Int Int -> Bool
eqConsistencyProp =  eqConsistencyTest

eqTransitiveProp :: Array Int Int -> Array Int Int -> Array Int Int -> Bool
eqTransitiveProp =  eqTransitiveTest

eqSymmetricProp :: Array Int Int -> Array Int Int -> Bool
eqSymmetricProp =  eqSymmetricTest

eqReflexiveProp :: Array Int Int -> Bool
eqReflexiveProp =  eqReflexiveTest

--------------------------------------------------------------------------------

{- Ord property. -}

ordAntisymmetryProp :: Array Int Int -> Array Int Int -> Bool
ordAntisymmetryProp =  ordAntisymmetryTest

ordTransitiveProp :: Array Int Int -> Array Int Int -> Array Int Int -> Bool
ordTransitiveProp =  ordTransitiveTest

ordTotalityProp :: Array Int Int -> Array Int Int -> Bool
ordTotalityProp =  ordTotalityTest

lexicographicOrdProp :: Long (Array Int Int) -> Long (Array Int Int) -> Bool
lexicographicOrdProp (Long xs) (Long ys) = lexicographicOrdTest xs ys

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp :: Char -> Array Int Char -> Bool
basicLinearProp =  basicLinearTest

deconstructionLinearProp :: Array Int Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp :: Char -> Array Int Char -> Bool
constructionLinearProp =  constructionLinearTest

reverseProp :: Array Int Char -> Bool
reverseProp =  reverseTest

replicateProp :: TestLinear2 Array Int Char
replicateProp =  replicateTest

concatProp :: Array Int Char -> Bool
concatProp =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: Char -> TestSplit2 Array Int Char
splitProp =  splitTest . (>)

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed2 Array Int Char
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed2 Array Int Char
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed2 Array Int Char
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Medium (Array Int Char) -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Set property. -}

setProp :: TestSet1 (Array Int) Char
setProp =  setTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate (Array Int Int)
estimateProp =  estimateTest


