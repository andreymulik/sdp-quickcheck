{- |
    Module      :  Test.SDP.Linear
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
    
    @Test.SDP.Linear@ provides basic test suite for 'Linear' class.
-}
module Test.SDP.Linear
(
  -- * Default Linear test
  TestLinear, TestLinear1, TestLinear2, linearTest,
  
  -- ** Particular tests
  deconstructionLinearTest, constructionLinearTest, basicLinearTest,
  replicateTest, reverseTest, concatTest
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Linear

default ()

--------------------------------------------------------------------------------

-- | 'TestLinear' is service type synonym for more comfortable quickCheck using.
type TestLinear  l e = Int -> e -> l -> Bool

-- | 'TestLinear1' is service type synonym for more comfortable quickCheck using.
type TestLinear1 f e = Int -> e -> f e -> Bool

-- | 'TestLinear2' is service type synonym for more comfortable quickCheck using.
type TestLinear2 f i e = Int -> e -> f i e -> Bool

--------------------------------------------------------------------------------

{- |
  'basicLinearTest' checks relations of 'isNull', 'lzero', 'single' and
  'fromList'.
-}
basicLinearTest :: (Linear l e, Eq l) => e -> l -> Bool
basicLinearTest e line = and
  [
    isNull (   lzero    `asTypeOf` line),
    isNull (fromList [] `asTypeOf` line),
    
    single  e == (fromList [e] `asTypeOf` line),
    not $ isNull (  single e   `asTypeOf` line),
    
    fromList (listL line) == line
  ]

{- |
  'deconstructionLinearTest' checks relations of 'isNull', 'head', 'last',
  'init' and 'tail'.
-}
deconstructionLinearTest :: (Linear l e, Eq e) => l -> Bool
deconstructionLinearTest line = and
  [
    isNull line || head line == head (listL line),
    isNull line || last line == last (listL line),
    
    isNull line || listL (init line) == init (listL line),
    isNull line || listL (tail line) == tail (listL line)
  ]

{- |
  'constructionLinearTest' checks relations of 'toHead', 'toLast' and
  'fromList'.
-}
constructionLinearTest :: (Linear l e, Eq l) => e -> l -> Bool
constructionLinearTest e line = and
  [
    toLast line e == fromList (listL line ++ [e]),
    toHead e line == fromList (e : listL line),
    
    not . isNull $ toHead e line,
    not . isNull $ toLast line e
  ]

-- | 'reverseTest' checks rules of 'reverse', 'listL' and 'listR'.
reverseTest :: (Linear l e, Eq e) => l -> Bool
reverseTest line = and
  [
    reverse (listL line) == listL (reverse line),
    reverse (listL line) == listR line
  ]

-- | 'replicateTest' checks rules of 'replicate'.
replicateTest :: (Linear l e, Eq l, Bordered l i) => Int -> e -> l -> Bool
replicateTest n e line = and
  [
    line' == fromList (replicate n e),
    n < 0 || sizeOf line' == n,
    not $ n > 0 && isNull line'
  ]
  where
    line' = (replicate n e) `asTypeOf` line

-- | 'concatTest' checks rules of ('++') and 'concat'.
concatTest :: (Linear l e, Eq e, Eq l) => l -> Bool
concatTest line = and
  [
    listL line ++ listR line == listL (line ++ reverse line),
    
    Z ++ line == line,
    line ++ Z == line,
    
    concat [line, reverse line] == fromList (concat [listL line, listR line]),
    concat [line, reverse line] == line ++ reverse line
  ]

-- | 'linearTest' is complex test, that includes all ther tests.
linearTest :: (Linear l e, Eq e, Eq l, Bordered l i) => Int -> e -> l -> Bool
linearTest n e line = and
  [
    basicLinearTest e line,
    deconstructionLinearTest line,
    constructionLinearTest e line,
    replicateTest n e line,
    reverseTest line,
    concatTest line
  ]




