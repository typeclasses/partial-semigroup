{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | This module defines the 'PartialSemigroup' instance for 'Identity'. -}

module Data.PartialSemigroup.Identity where

import {-# SOURCE #-} Data.PartialSemigroup (PartialSemigroup ((<>?)))

import Data.Functor.Identity (Identity (..))

instance PartialSemigroup a => PartialSemigroup (Identity a)
  where
    Identity x <>? Identity y = Identity <$> (x <>? y)
