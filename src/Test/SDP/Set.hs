{- |
    Module      :  Test.SDP.Set
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
    
    @Test.SDP.Set@ provides basic test quite for 'Set' class.
-}
module Test.SDP.Set
(
  -- * Set test
  TestSet, TestSet1, setTest,
  
  -- * Particular tests
  basicSetTest, insdelSetTest, lookupSetTest, unintSetTest, diffSetTest,
  elemSetTest
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Linear
import SDP.Set

default ()

--------------------------------------------------------------------------------

-- | TestSet  is service type synonym for more comfortable quickCheck using.
type TestSet  s o = o -> s -> s -> Bool

-- | TestSet1 is service type synonym for more comfortable quickCheck using.
type TestSet1 s o = o -> s o -> s o -> Bool

--------------------------------------------------------------------------------

{- |
  'basicSetTest' checks relations of 'set', ('/?\') and ('\?/').
  Note that basicSetTest requires any @('Set' s o) => s@, not necessarily a set
  (may contain any data).
-}
basicSetTest :: (Set s o, Eq s) => s -> Bool
basicSetTest sx = and
    [
      isNull sx == isNull sx',
      
      set sx' == sx',
      
      (sx' \?/ sx') /= (sx' /?\ sx')
    ]
  where
    sx' = set sx

{- |
  'insdelSetTest' checks rules of 'insert' and 'delete'.
  Note that 'insdelSetTest' requires a set, not any @('Set' s o) => s@.
-}
insdelSetTest :: (Set s o, Eq s) => o -> s -> Bool
insdelSetTest e sx' = and
  [
    (insert e sx' == sx') || not (member e sx'),
    (delete e sx' == sx') ||     (member e sx')
  ]

{- |
  'unintSetTest' checks the laws of union ('\/') and intersection ('/\').
  Note that unintSetTest requires any @('Set' s o) => s@, not necessarily a set
  (may contain any data).
-}
unintSetTest :: (Set s o, Linear s o, Eq o) => s -> s -> Bool
unintSetTest sx' sy' = and
    [
      (is `isSubseqOf` sx') && (is  `isSubseqOf` sy') && (is `isSubseqOf` un),
      (sx' `isSubseqOf` un) && (sy' `isSubseqOf` un)
    ]
  where
    is = sx' /\  sy'
    un = sx' \/  sy'

{- |
  'diffSetTest' checks laws of difference ('\\') and symmetric difference
  ('\^/'). Note that diffSetTest requires a set, not any @('Set' s o) => s@
-}
diffSetTest :: (Set s o, Linear s o, Ord o) => s -> s -> Bool
diffSetTest sx' sy' = and
    [
      (cp `isSubseqOf` sx') && (isNull cp || not (cp `isSubseqOf` sy')) && (cp `isSubseqOf` un),
      (isNull sd && isNull is || sd /?\ is) && (sd `isSubseqOf` un)
    ]
  where
    is = sx' /\  sy'
    un = sx' \/  sy'
    cp = sx' \\  sy'
    sd = sx' \^/ sy'

{- |
  'elemSetTest' checks relations of 'member' and 'isSubseqOf'.
  Note that elemSetTest requires any @('Set' s o) => s@, not necessarily a set
  (may contain any data).
-}
elemSetTest :: (Set s o, Linear s o, Eq o) => o -> s -> Bool
elemSetTest e sx = and
    [
      (e' `isSubseqOf` sx) == member e sx',
      (e' `isSubseqOf` sx) == member e sx'
    ]
  where
    sx' = set sx; e' = single e

{- |
  'lookupSetTest' checks relations of 'lookupLT', 'lookupGT', 'lookupLE' and
  'lookupGE'. Note that lookupSetTest requires a set, not any @('Set' s o) => s@.
-}
lookupSetTest :: (Set s o, Linear s o, Ord o) => o -> s -> Bool
lookupSetTest e sx = and
    [
      lookupLT e sx == lookupLT e (listL sx),
      lookupGT e sx == lookupGT e (listL sx),
      lookupLE e sx == lookupLE e (listL sx),
      lookupGE e sx == lookupGE e (listL sx),
      
      case lookupLT e sx of {Nothing -> True; Just x -> x <  e},
      case lookupGT e sx of {Nothing -> True; Just x -> x >  e},
      case lookupLE e sx of {Nothing -> True; Just x -> x <= e},
      case lookupGE e sx of {Nothing -> True; Just x -> x >= e}
    ]

{- |
  'setTest' is complex test, that includes all other tests.
  Note that setTest requires any @('Set' s o) => s@, not necessarily a set (may
  contain any data).
-}
setTest :: (Set s o, Linear s o, Ord s, Ord o) => o -> s -> s -> Bool
setTest e xs ys = and
    [
      unintSetTest  sx sy,
      diffSetTest   sx sy,
      basicSetTest     xs,
      insdelSetTest e  sx,
      lookupSetTest e  sx,
      elemSetTest   e  xs
    ]
  where
    sx = set xs
    sy = set ys




