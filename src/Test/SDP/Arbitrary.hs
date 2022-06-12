{-# LANGUAGE MagicHash, TypeOperators, FlexibleContexts, UndecidableInstances #-}

{- |
    Module      :  Test.SDP.Arbitrary
    Copyright   :  (c) Andrey Mulik 2020
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
  
  SArray#, SBytes#, AnyBorder (..), AnyChunks
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

instance (Arbitrary i, Arbitrary i') => Arbitrary (i' :& i)
  where
    -- [internal]: QuickCheck-2.10 doesn't provide 'applyArbitrary2'
    arbitrary = liftA2 (:&) arbitrary arbitrary

--------------------------------------------------------------------------------

instance (Arbitrary e) => Arbitrary (SArray# e)
  where
    arbitrary = fromList <$> arbitrary

instance (Unboxed e, Arbitrary e) => Arbitrary (SBytes# e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

-- TODO: rewrite as arbitrary chunk list generator (needs sdp improvements).

instance (Bordered1 rep Int e, Linear1 rep e, Arbitrary e) => Arbitrary (AnyChunks rep e)
  where
    arbitrary = fromList <$> arbitrary

instance (Index i, Bordered1 rep Int e, Arbitrary (rep e)) => Arbitrary (AnyBorder rep i e)
  where
    arbitrary = (\ es -> uncurry AnyBorder (defaultBounds $ sizeOf es) es) <$> arbitrary

