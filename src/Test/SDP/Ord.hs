{- |
    Module      :  Test.SDP.Ord
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @Test.SDP.Ord@ provides basic test suite for 'Ord' instances.
-}
module Test.SDP.Ord
(
  -- * Ord test
  TestOrd, ordTest,
  
  -- ** Partial tests
  ordAntisymmetryTest, ordTransitiveTest, ordTotalityTest,
  
  -- ** Lexicographic test
  lexicographicOrdTest
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Linear

default ()

--------------------------------------------------------------------------------

-- | TestOrd is service type synonym for more comfortable quickCheck using.
type TestOrd l = l -> l -> l -> Bool

--------------------------------------------------------------------------------

-- | ordTest is basic test suite for 'Ord' instances.
ordTest :: Ord l => l -> l -> l -> Bool
ordTest xs ys zs = and
  [
    -- antisymmetry
    ordAntisymmetryTest xs ys,
    
    -- transitivity
    ordTransitiveTest xs ys zs,
    
    -- totality
    ordTotalityTest xs ys
  ]

--------------------------------------------------------------------------------

-- | lexicographicOrdTest checks 'Linear' structures for lexicographic order.
lexicographicOrdTest :: (Linear l e, Ord l, Ord e) => l -> l -> Bool
lexicographicOrdTest xs ys = (xs <=> ys) == (listL xs <=> listL ys)

--------------------------------------------------------------------------------

-- | 'ordAntisymmetryTest' checks if 'Ord' instances is antisymmetric.
ordAntisymmetryTest :: Ord l => l -> l -> Bool
ordAntisymmetryTest xs ys = (xs <= ys && ys <= xs) <= (xs == ys)

-- | 'ordAntisymmetryTest' checks if 'Ord' instances is transitive.
ordTransitiveTest :: Ord l => l -> l -> l -> Bool
ordTransitiveTest xs ys zs = (xs <= ys && ys <= zs) <= (xs <= zs)

-- | 'ordTotalityTest' checks if 'Ord' instances is total.
ordTotalityTest :: Ord l => l -> l -> Bool
ordTotalityTest xs ys = (xs <= ys) /= (xs > ys)


