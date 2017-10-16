{-# LANGUAGE MonadComprehensions #-}

module Test.PartialSemigroup
  ( assoc
  ) where

-- partial-semigroup
import Data.PartialSemigroup

-- hedgehog
import Hedgehog

-- base
import Control.Monad (mzero)

{- | The partial semigroup associativity axiom:

For all @x@, @y@, @z@: If @appendMaybe x y = Just xy@ and
@appendMaybe y z = Just yz@, then @appendMaybe x yz = appendMaybe xy z@. -}

assoc :: (PartialSemigroup a, Eq a, Show a) => Gen a -> Property
assoc gen = property $ do

  x <- forAll gen
  y <- forAll gen
  z <- forAll gen

  xy <- maybe mzero pure (appendMaybe x y)
  yz <- maybe mzero pure (appendMaybe y z)

  appendMaybe x yz === appendMaybe xy z
