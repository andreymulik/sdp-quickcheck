{-# LANGUAGE TypeOperators, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE Safe, CPP, MagicHash #-}

#ifdef SDP_QUICKCHECK_QUALIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{- |
    Module      :  Test.SDP.Arbitrary
    Copyright   :  (c) Andrey Mulik 2020-2023
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @Test.SDP.Arbitrary@ is service module that provides 'Arbitrary' instances
    for @sdp@ structures.
-}
module Test.SDP.Arbitrary
(
  -- * Exports
  module Test.QuickCheck,
  
  SArray#, SBytes#, AnyBorder, AnyChunks
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Unboxed

import SDP.Templates.AnyBorder
import SDP.Templates.AnyChunks
import SDP.Prim.SArray
import SDP.Prim.SBytes

import Test.QuickCheck

default ()

--------------------------------------------------------------------------------

instance Arbitrary E where arbitrary = return E

instance (Arbitrary i, Arbitrary is) => Arbitrary (is :& i)
  where
    -- [internal]: QuickCheck-2.10 doesn't provide 'applyArbitrary2'
    arbitrary = liftA2 (:&) arbitrary arbitrary

--------------------------------------------------------------------------------

instance Arbitrary e => Arbitrary (SArray# e)
  where
    arbitrary = do n <- getSize; fromListN n <$> vector n

instance (Unboxed e, Arbitrary e) => Arbitrary (SBytes# e)
  where
    arbitrary = fromList <$> arbitrary

instance (Nullable1 rep e, Arbitrary (rep e)) => Arbitrary (AnyChunks rep e)
  where
    arbitrary = do n <- getSize; fromChunks <$> vectorOf n arbitrary

instance (Index i, Estimate1 rep e, Arbitrary (rep e)) => Arbitrary (AnyBorder rep i e)
  where
    arbitrary = do
      es <- arbitrary
      let (l, u) = defaultBounds (sizeOf es)
      return (AnyBorder l u es)

--------------------------------------------------------------------------------

#if MIN_VERSION_QuickCheck(2,10,0)

-- | Since @QuickCheck-2.10@.
instance Arbitrary1 SArray#
  where
    liftArbitrary gen = do n <- getSize; fromListN n <$> vectorOf n gen

#ifdef SDP_QUICKCHECK_QUALIFIED_CONSTRAINTS
-- | Since @QuickCheck-2.10@, require @GHC 8.6+@.
instance (Nullable' rep, Arbitrary1 rep) => Arbitrary1 (AnyChunks rep)
  where
    liftArbitrary gen = do n <- getSize; fromChunks <$> vectorOf n (liftArbitrary gen)

-- | Since @QuickCheck-2.10@, require @GHC 8.6+@.
instance (Index i, Nullable' rep, Estimate' rep, Arbitrary1 rep)
      => Arbitrary1 (AnyBorder rep i)
  where
    liftArbitrary gen = do
      es <- liftArbitrary gen
      let (l, u) = defaultBounds (sizeOf es)
      return (AnyBorder l u es)
#endif

#else

{-
  getSize available since QuickCheck-2.10, for QuickCheck-2.9 I only can
  generate an arbitrary size
-}
getSize :: Gen Int
getSize =  abs <$> arbitrary

#endif

