module Main where

import Test.Framework.Providers.QuickCheck2
import Test.Framework

import SDP.ByteList

import Test.SDP.Arbitrary ()
import Test.SDP

default ()

--------------------------------------------------------------------------------

main :: IO ()
main =  defaultMain
  [
    -- Eq tests
    testProperty "bytelist-eq-consistency   " eqConsistencyProp,
    testProperty "bytelist-eq-transitive    " eqTransitiveProp,
    testProperty "bytelist-eq-symmetric     " eqSymmetricProp,
    testProperty "bytelist-eq-reflexive     " eqReflexiveProp,
    
    -- Ord tests
    testProperty "bytelist-ord-antisymmetry " ordAntisymmetryProp,
    testProperty "bytelist-ord-transitive   " ordTransitiveProp,
    testProperty "bytelist-ord-totality     " ordTotalityProp,
    testProperty "bytelist-lexicographic    " lexicographicOrdProp,
    
    -- linear tests
    testProperty "bytelist-linear-basic     " basicLinearProp,
    testProperty "bytelist-linear-decons    " deconstructionLinearProp,
    testProperty "bytelist-linear-cons      " constructionLinearProp,
    testProperty "bytelist-linear-reverse   " reverseProp,
    testProperty "bytelist-linear-concat    " concatProp,
    
    -- split test
    testProperty "bytelist-split            " splitProp,
    
    -- indexed tests
    testProperty "bytelist-indexed-basic    " basicIndexedProp,
    testProperty "bytelist-indexed-assoc    " assocIndexedProp,
    testProperty "bytelist-indexed-read     " readIndexedProp,
    
    -- sort test
    testProperty "bytelist-sort             " sortProp,
    
    -- set test
    testProperty "bytelist-set              " setProp,
    
    -- estimate test
    testProperty "bytelist-estimate         " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqConsistencyProp :: ByteList Int Int -> ByteList Int Int -> Bool
eqConsistencyProp =  eqConsistencyTest

eqTransitiveProp :: ByteList Int Int -> ByteList Int Int
                 -> ByteList Int Int -> Bool
eqTransitiveProp =  eqTransitiveTest

eqSymmetricProp :: ByteList Int Int -> ByteList Int Int -> Bool
eqSymmetricProp =  eqSymmetricTest

eqReflexiveProp :: ByteList Int Int -> Bool
eqReflexiveProp =  eqReflexiveTest

--------------------------------------------------------------------------------

{- Ord property. -}

ordAntisymmetryProp :: ByteList Int Int -> ByteList Int Int -> Bool
ordAntisymmetryProp =  ordAntisymmetryTest

ordTransitiveProp :: ByteList Int Int -> ByteList Int Int -> ByteList Int Int -> Bool
ordTransitiveProp =  ordTransitiveTest

ordTotalityProp :: ByteList Int Int -> ByteList Int Int -> Bool
ordTotalityProp =  ordTotalityTest

lexicographicOrdProp :: Long (ByteList Int Int) -> Long (ByteList Int Int) -> Bool
lexicographicOrdProp (Long xs) (Long ys) = lexicographicOrdTest xs ys

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp :: Char -> ByteList Int Char -> Bool
basicLinearProp =  basicLinearTest

deconstructionLinearProp :: ByteList Int Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp :: Char -> ByteList Int Char -> Bool
constructionLinearProp =  constructionLinearTest

reverseProp :: ByteList Int Char -> Bool
reverseProp =  reverseTest

replicateProp :: TestLinear2 ByteList Int Char
replicateProp =  replicateTest

concatProp :: ByteList Int Char -> Bool
concatProp =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: Char -> TestSplit2 ByteList Int Char
splitProp =  splitTest . (>)

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed2 ByteList Int Char
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed2 ByteList Int Char
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed2 ByteList Int Char
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Medium (ByteList Int Char) -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Set property. -}

setProp :: TestSet1 (ByteList Int) Char
setProp =  setTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate (ByteList Int Int)
estimateProp =  estimateTest

