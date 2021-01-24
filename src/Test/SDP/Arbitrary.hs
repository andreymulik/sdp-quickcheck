{-# LANGUAGE MagicHash, TypeOperators, UndecidableInstances #-}

{- |
    Module      :  Test.SDP.Arbitrary
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @Test.SDP.Arbitrary@ is service module that provides 'Arbitrary' instances
    for SDP structures.
-}
module Test.SDP.Arbitrary
(
  -- * Exports
  module Test.QuickCheck,
  
  SArray#, SBytes#, AnyBorder (..), AnyChunks (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Unboxed

import Test.QuickCheck

import SDP.Prim.SArray
import SDP.Prim.SBytes

import SDP.Templates.AnyBorder
import SDP.Templates.AnyChunks

default ()

--------------------------------------------------------------------------------

instance Arbitrary E where arbitrary = return E

instance (Arbitrary i, Arbitrary i') => Arbitrary (i' :& i)
  where
    arbitrary = applyArbitrary2 (:&)

--------------------------------------------------------------------------------

instance (Arbitrary e) => Arbitrary (SArray# e)
  where
    arbitrary = fromList <$> arbitrary

instance (Unboxed e, Arbitrary e) => Arbitrary (SBytes# e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

-- TODO: rewrite as arbitrary chunk list generator (needs sdp improvements).

{-
instance (Arbitrary (rep e)) => Arbitrary (AnyChunks rep e)
  where
    arbitrary = AnyChunks <$> arbitrary
-}
instance (Bordered1 rep Int e, Linear1 rep e, Arbitrary e) => Arbitrary (AnyChunks rep e)
  where
    arbitrary = fromList <$> arbitrary

instance (Index i, Bordered1 rep Int e, Arbitrary (rep e)) => Arbitrary (AnyBorder rep i e)
  where
    arbitrary = (\ es -> uncurry AnyBorder (defaultBounds $ sizeOf es) es) <$> arbitrary

