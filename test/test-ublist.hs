module Main where

import Test.Framework.Providers.QuickCheck2
import Test.Framework

import SDP.ByteList.Ublist

import Test.SDP.Arbitrary ()
import Test.SDP

default ()

--------------------------------------------------------------------------------

main :: IO ()
main =  defaultMain
  [
    -- Eq tests
    testProperty "ublist-eq-consistency   " eqConsistencyProp,
    testProperty "ublist-eq-transitive    " eqTransitiveProp,
    testProperty "ublist-eq-symmetric     " eqSymmetricProp,
    testProperty "ublist-eq-reflexive     " eqReflexiveProp,
    
    -- Ord tests
    testProperty "ublist-ord-antisymmetry " ordAntisymmetryProp,
    testProperty "ublist-ord-transitive   " ordTransitiveProp,
    testProperty "ublist-ord-totality     " ordTotalityProp,
    testProperty "ublist-lexicographic    " lexicographicOrdProp,
    
    -- linear tests
    testProperty "ublist-linear-basic     " basicLinearProp,
    testProperty "ublist-linear-decons    " deconstructionLinearProp,
    testProperty "ublist-linear-cons      " constructionLinearProp,
    testProperty "ublist-linear-reverse   " reverseProp,
    testProperty "ublist-linear-concat    " concatProp,
    
    -- split test
    testProperty "ublist-split            " splitProp,
    
    -- indexed tests
    testProperty "ublist-indexed-basic    " basicIndexedProp,
    testProperty "ublist-indexed-assoc    " assocIndexedProp,
    testProperty "ublist-indexed-read     " readIndexedProp,
    
    -- sort test
    testProperty "ublist-sort             " sortProp,
    
    -- set test
    testProperty "ublist-set              " setProp,
    
    -- estimate test
    testProperty "ublist-estimate         " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqConsistencyProp :: Ublist Int -> Ublist Int -> Bool
eqConsistencyProp =  eqConsistencyTest

eqTransitiveProp :: Ublist Int -> Ublist Int -> Ublist Int -> Bool
eqTransitiveProp =  eqTransitiveTest

eqSymmetricProp :: Ublist Int -> Ublist Int -> Bool
eqSymmetricProp =  eqSymmetricTest

eqReflexiveProp :: Ublist Int -> Bool
eqReflexiveProp =  eqReflexiveTest

--------------------------------------------------------------------------------

{- Ord property. -}

ordAntisymmetryProp :: Ublist Int -> Ublist Int -> Bool
ordAntisymmetryProp =  ordAntisymmetryTest

ordTransitiveProp :: Ublist Int -> Ublist Int -> Ublist Int -> Bool
ordTransitiveProp =  ordTransitiveTest

ordTotalityProp :: Ublist Int -> Ublist Int -> Bool
ordTotalityProp =  ordTotalityTest

lexicographicOrdProp :: Long (Ublist Int) -> Long (Ublist Int) -> Bool
lexicographicOrdProp (Long xs) (Long ys) = lexicographicOrdTest xs ys

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp :: Char -> Ublist Char -> Bool
basicLinearProp =  basicLinearTest

deconstructionLinearProp :: Ublist Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp :: Char -> Ublist Char -> Bool
constructionLinearProp =  constructionLinearTest

reverseProp :: Ublist Char -> Bool
reverseProp =  reverseTest

replicateProp :: TestLinear1 Ublist Char
replicateProp =  replicateTest

concatProp :: Ublist Char -> Bool
concatProp =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: Char -> TestSplit1 Ublist Char
splitProp =  splitTest . (>)

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed1 Ublist Int Char
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed1 Ublist Int Char
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed1 Ublist Int Char
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Medium (Ublist Char) -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Set property. -}

setProp :: TestSet1 Ublist Char
setProp =  setTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate (Ublist Int)
estimateProp =  estimateTest


