-- | Utilities for testing partial semigroups using the @hedgehog@ property
-- testing library.
module Test.PartialSemigroup.Hedgehog
  ( assoc,
    identity,
  )
where

import Data.PartialSemigroup (PartialMonoid (..), PartialSemigroup (..))
import Hedgehog (Gen, Property, forAll, property, (===))

-- | The partial semigroup associativity axiom:
--
-- For all @x@, @y@, @z@: If @x '<>?' y = 'Just' xy@ and @y '<>?' z = 'Just' yz@,
-- then @x '<>?' yz = xy '<>?' z@.
assoc :: (PartialSemigroup a, Eq a, Show a) => Gen a -> Property
assoc gen = property $ do
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen

  sequence_ $
    do
      xy <- x <>? y
      yz <- y <>? z

      return (x <>? yz === xy <>? z)

-- | The partial monoid identity axiom:
--
-- For all @x@, @y@: @'pmempty' '<>?' x = x '<>?' 'pmempty'@ and if @'pmempty
-- '<>?' x = 'Just' y@, @x = y@.
identity :: (PartialMonoid a, Eq a, Show a) => Gen a -> Property
identity gen = property $ do
  x <- forAll gen

  -- Both are either Nothing or Just y.
  pmempty <>? x === x <>? pmempty

  -- If they are Just y, then y == x.
  sequence_ $
    do
      oneX <- pmempty <>? x
      return (oneX === x)
