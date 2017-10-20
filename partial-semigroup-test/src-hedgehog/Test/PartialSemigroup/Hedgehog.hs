{- | Utilities for testing partial semigroups using the @hedgehog@ property
testing library. -}

module Test.PartialSemigroup.Hedgehog
  ( assoc
  ) where

-- partial-semigroup
import Data.PartialSemigroup

-- hedgehog
import Hedgehog (Gen, Property, forAll, (===), property)

-- base
import Control.Applicative (Alternative, empty)

{- | The partial semigroup associativity axiom:

For all @x@, @y@, @z@: If @x '<>?' y = 'Just' xy@ and @y '<>?' z = 'Just' yz@,
then @x '<>?' yz = xy '<>?' z@. -}

assoc :: (PartialSemigroup a, Eq a, Show a) => Gen a -> Property
assoc gen = property $ do

  x <- forAll gen
  y <- forAll gen
  z <- forAll gen

  xy <- liftMaybe (x <>? y)
  yz <- liftMaybe (y <>? z)

  x <>? yz === xy <>? z

liftMaybe :: Alternative f => Maybe a -> f a
liftMaybe = maybe empty pure
