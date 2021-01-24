{- |
    Module      :  Test.SDP.Split
    Copyright   :  (c) Andrey Mulik 2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @Test.SDP.Split@ is service module that provides 'Split' tests for SDP
    structures.
-}
module Test.SDP.Split
(
  -- * Default Split test
  TestSplit, TestSplit1, TestSplit2, splitTest,
  
  -- ** Particular tests
  basicSplitTest,
  whileSplitTest
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Linear

default ()

--------------------------------------------------------------------------------

-- | 'TestSplit' is service type synonym for more comfortable quickCheck using.
type TestSplit s = Int -> s -> Bool

-- | 'TestSplit1' is service type synonym for more comfortable quickCheck using.
type TestSplit1 s e = Int -> s e -> Bool

-- | 'TestSplit2' is service type synonym for more comfortable quickCheck using.
type TestSplit2 s i e = Int -> s i e -> Bool

--------------------------------------------------------------------------------

{- |
  @'splitTest' f n xs@ is default 'Split' test, where @f@ is arbitrary predicate
  (e.g. "Test.SDP.Gen.orderA").
-}
splitTest :: (Split s e, Eq e, Eq s, Bordered s i) => (e -> Bool) -> TestSplit s
splitTest f n xs = and
  [
    basicSplitTest n xs,
    whileSplitTest f xs
  ]

{- |
  'basicSplitTest' checks 'take', 'drop', 'sans', 'keep', 'split' and 'divide'
  correctness and relations.
-}
basicSplitTest :: (Split s e, Eq e, Eq s, Bordered s i) => TestSplit s
basicSplitTest n xs = and
    [
      split  n xs == (tx, dx),
      divide n xs == (sx, kx),
      
      listL tx == ty, listL dx == dy,
      listL sx == sy, listL kx == ky,
      
      -- additional tests
      lx == sizeOf tx + sizeOf dx,
      lx == sizeOf sx + sizeOf kx
    ]
  where
    lx = sizeOf xs
    ys = listL  xs
    
    tx = take n xs; ty = take n ys
    dx = drop n xs; dy = drop n ys
    sx = sans n xs; sy = sans n ys
    kx = keep n xs; ky = keep n ys

{- |
  'whileSplitTest' checks 'takeWhile', 'dropWhile', 'takeEnd', 'dropEnd',
  'spanl', 'spanr', 'breakl' and 'breakr' correctness and relations.
-}
whileSplitTest :: (Split s e, Eq e, Eq s, Bordered s i) => (e -> Bool) -> s -> Bool
whileSplitTest f xs = and
    [
      spanl f xs == (tx, dx), breakl f xs == (ntx, ndx),
      spanr f xs == (sx, kx), breakr f xs == (nsx, nkx),
      
      listL tx == ty, listL dx == dy,
      listL sx == sy, listL kx == ky,
      
      -- additional tests
      lx == sizeOf  tx + sizeOf  dx,
      lx == sizeOf  sx + sizeOf  kx,
      lx == sizeOf ntx + sizeOf ndx,
      lx == sizeOf nsx + sizeOf nkx
    ]
  where
    lx = sizeOf xs
    ys = listL  xs
    
    tx = takeWhile f xs; ty = takeWhile f ys; ntx = takeWhile (not . f) xs
    dx = dropWhile f xs; dy = dropWhile f ys; ndx = dropWhile (not . f) xs
    sx = dropEnd   f xs; sy = dropEnd   f ys; nsx = dropEnd   (not . f) xs
    kx = takeEnd   f xs; ky = takeEnd   f ys; nkx = takeEnd   (not . f) xs

