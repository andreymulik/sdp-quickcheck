{- |
    Module      :  Test.SDP.Eq
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @Test.SDP.Eq@ provides basic test suite for 'Eq' instances.
-}
module Test.SDP.Eq
(
  -- * Eq test
  TestEq, eqTest,
  
  -- ** Partial tests
  eqTransitiveTest, eqSymmetricTest, eqReflexiveTest, eqConsistencyTest
)
where

default ()

--------------------------------------------------------------------------------

-- | TestEq is service type synonym for more comfortable quickCheck using.
type TestEq l = l -> l -> l -> Bool

--------------------------------------------------------------------------------

-- | 'eqTest' is basic test suite for 'Eq' instances.
eqTest :: Eq l => l -> l -> l -> Bool
eqTest xs ys zs = and
  [
    -- transitive
    eqTransitiveTest xs ys zs,
    
    -- symmetric
    eqSymmetricTest xs ys,
    
    -- reflexive
    eqReflexiveTest xs,
    
    -- consistent
    eqConsistencyTest xs ys
  ]

--------------------------------------------------------------------------------

-- | 'eqTransitiveTest' checks if 'Eq' instance definition is transitive.
eqTransitiveTest :: Eq l => l -> l -> l -> Bool
eqTransitiveTest xs ys zs = (xs == ys && ys == zs) <= (xs == zs)

-- | 'eqSymmetricTest' checks if 'Eq' instance definition is symmetric.
eqSymmetricTest :: Eq l => l -> l -> Bool
eqSymmetricTest xs ys = (xs == ys) == (ys == xs)

-- | 'eqReflexiveTest' checks if 'Eq' instance definition is reflexive.
eqReflexiveTest :: Eq l => l -> Bool
eqReflexiveTest xs = xs == xs

-- | 'eqConsistencyTest' checks 'Eq' instance consistency.
eqConsistencyTest :: Eq l => l -> l -> Bool
eqConsistencyTest xs ys = (xs == ys) /= (xs /= ys)



