module Data.PartialSemigroup where

class PartialSemigroup a
  where
    (<>?) :: a -> a -> Maybe a
