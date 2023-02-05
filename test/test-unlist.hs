module Main where

import Test.Framework.Providers.QuickCheck2
import Test.Framework

import SDP.Unrolled.Unlist

import Test.SDP.Arbitrary ()
import Test.SDP

default ()

--------------------------------------------------------------------------------

main :: IO ()
main =  defaultMain
  [
    -- Eq tests
    testProperty "unlist-eq-consistency   " eqConsistencyProp,
    testProperty "unlist-eq-transitive    " eqTransitiveProp,
    testProperty "unlist-eq-symmetric     " eqSymmetricProp,
    testProperty "unlist-eq-reflexive     " eqReflexiveProp,
    
    -- Ord tests
    testProperty "unlist-ord-antisymmetry " ordAntisymmetryProp,
    testProperty "unlist-ord-transitive   " ordTransitiveProp,
    testProperty "unlist-ord-totality     " ordTotalityProp,
    testProperty "unlist-lexicographic    " lexicographicOrdProp,
    
    -- linear tests
    testProperty "unlist-linear-basic     " basicLinearProp,
    testProperty "unlist-linear-decons    " deconstructionLinearProp,
    testProperty "unlist-linear-cons      " constructionLinearProp,
    testProperty "unlist-linear-reverse   " reverseProp,
    testProperty "unlist-linear-concat    " concatProp,
    
    -- split test
    testProperty "unlist-split            " splitProp,
    
    -- indexed tests
    testProperty "unlist-indexed-basic    " basicIndexedProp,
    testProperty "unlist-indexed-assoc    " assocIndexedProp,
    testProperty "unlist-indexed-read     " readIndexedProp,
    
    -- sort test
    testProperty "unlist-sort             " sortProp,
    
    -- set test
    testProperty "unlist-set              " setProp,
    
    -- estimate test
    testProperty "unlist-estimate         " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqConsistencyProp :: Unlist Int -> Unlist Int -> Bool
eqConsistencyProp =  eqConsistencyTest

eqTransitiveProp :: Unlist Int -> Unlist Int -> Unlist Int -> Bool
eqTransitiveProp =  eqTransitiveTest

eqSymmetricProp :: Unlist Int -> Unlist Int -> Bool
eqSymmetricProp =  eqSymmetricTest

eqReflexiveProp :: Unlist Int -> Bool
eqReflexiveProp =  eqReflexiveTest

--------------------------------------------------------------------------------

{- Ord property. -}

ordAntisymmetryProp :: Unlist Int -> Unlist Int -> Bool
ordAntisymmetryProp =  ordAntisymmetryTest

ordTransitiveProp :: Unlist Int -> Unlist Int -> Unlist Int -> Bool
ordTransitiveProp =  ordTransitiveTest

ordTotalityProp :: Unlist Int -> Unlist Int -> Bool
ordTotalityProp =  ordTotalityTest

lexicographicOrdProp :: Long (Unlist Int) -> Long (Unlist Int) -> Bool
lexicographicOrdProp (Long xs) (Long ys) = lexicographicOrdTest xs ys

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp :: Char -> Unlist Char -> Bool
basicLinearProp =  basicLinearTest

deconstructionLinearProp :: Unlist Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp :: Char -> Unlist Char -> Bool
constructionLinearProp =  constructionLinearTest

reverseProp :: Unlist Char -> Bool
reverseProp =  reverseTest

replicateProp :: TestLinear1 Unlist Char
replicateProp =  replicateTest

concatProp :: Unlist Char -> Bool
concatProp =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: Char -> TestSplit1 Unlist Char
splitProp =  splitTest . (>)

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed1 Unlist Int Char
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed1 Unlist Int Char
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed1 Unlist Int Char
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Medium (Unlist Char) -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Set property. -}

setProp :: TestSet1 Unlist Char
setProp =  setTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate (Unlist Int)
estimateProp =  estimateTest


