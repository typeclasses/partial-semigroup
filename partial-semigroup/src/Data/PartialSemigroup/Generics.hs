-- | If a type derives 'Generic' and all of its fields have 'PartialSemigroup'
-- instances, you can get a 'PartialSemigroup' for free using
-- 'genericPartialSemigroupOp'.
--
-- == Example
--
-- For this demonstration we'll define a contrived example type @T@ with two
-- constructors, @A@ and @B@.
--
-- >>> data T = A String (Either String String) | B String deriving (Generic, Show)
--
-- And then define its 'PartialSemigroup' instance using
-- 'genericPartialSemigroupOp'.
--
-- >>> instance PartialSemigroup T where (<>?) = genericPartialSemigroupOp
--
-- This gives us an implementation of '<>?' which combines values only if they have
-- the same structure.
--
-- >>> A "s" (Left "x") <>? A "t" (Left "y")
-- Just (A "st" (Left "xy"))
--
-- >>> B "x" <>? B "y"
-- Just (B "xy")
--
-- For values that do /not/ have the same structure, '<>?' produces 'Nothing'.
--
-- >>> A "s" (Left "x") <>? A "t" (Right "y")
-- Nothing
--
-- >>> A "x" (Left "y") <>? B "z"
-- Nothing
module Data.PartialSemigroup.Generics
  ( -- * The generic PartialSemigroup operator
    genericPartialSemigroupOp,

    -- * Implementation details
    PartialSemigroupRep (..),

    -- * Re-exports
    Generic,
    PartialSemigroup (..),
  )
where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (Maybe (..))
import Data.PartialSemigroup
import GHC.Generics
  ( Generic,
    K1 (..),
    M1 (..),
    Rep,
    from,
    to,
    (:*:) (..),
    (:+:) (..),
  )

-- $setup
--
-- >>> :set -XDeriveGeneric
--
-- >>> import Data.Either (Either (..))
-- >>> import Data.String (String)
-- >>> import Text.Show (Show)

genericPartialSemigroupOp ::
  (Generic a, PartialSemigroupRep (Rep a)) =>
  a ->
  a ->
  Maybe a
genericPartialSemigroupOp x y =
  to <$> repPartialSemigroupOp (from x) (from y)

-- |
--
-- The class of generic type 'Rep's for which we can automatically derive
-- 'PartialSemigroup':
--
--   * 'K1' - a single value
--   * 'M1' - a value with some additional metadata (which we simply discard)
--   * ':+:' - sum types
--   * ':*:' - product types
class PartialSemigroupRep rep where
  repPartialSemigroupOp :: rep a -> rep a -> Maybe (rep a)

instance PartialSemigroup a => PartialSemigroupRep (K1 i a) where
  repPartialSemigroupOp (K1 x) (K1 y) = K1 <$> (x <>? y)

instance PartialSemigroupRep rep => PartialSemigroupRep (M1 i meta rep) where
  repPartialSemigroupOp (M1 x) (M1 y) = M1 <$> repPartialSemigroupOp x y

instance
  (PartialSemigroupRep rep1, PartialSemigroupRep rep2) =>
  PartialSemigroupRep (rep1 :*: rep2)
  where
  repPartialSemigroupOp (x1 :*: x2) (y1 :*: y2) =
    (:*:)
      <$> repPartialSemigroupOp x1 y1
      <*> repPartialSemigroupOp x2 y2

instance
  (PartialSemigroupRep rep1, PartialSemigroupRep rep2) =>
  PartialSemigroupRep (rep1 :+: rep2)
  where
  repPartialSemigroupOp (L1 x) (L1 y) = L1 <$> repPartialSemigroupOp x y
  repPartialSemigroupOp (R1 x) (R1 y) = R1 <$> repPartialSemigroupOp x y
  repPartialSemigroupOp _ _ = Nothing
