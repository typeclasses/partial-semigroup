{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.PartialSemigroup
  (
  -- * Partial semigroup
    PartialSemigroup (..)

  -- * Either
  -- $either
  , AppendLeft (..)
  , AppendRight (..)

  -- * Tuples
  -- $tuple

  -- * Concatenation
  , groupAndConcat
  , partialConcat
  , partialConcat1

  -- * Zipping
  , partialZip
  , partialZip1

  -- * Total semigroups
  -- $total
  , TotalSemigroup (..)

  ) where

import Control.Applicative   (ZipList (..), (<*>))
import Control.Monad         ((>>=))
import Data.Either           (Either (..))
import Data.Function         ((.))
import Data.Functor          ((<$>))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty    (NonEmpty (..), nonEmpty)
import Data.Maybe            (Maybe (..))
import Data.Semigroup        (Semigroup (..))
import Text.Show             (Show)

-- $setup
--
-- >>> :set -XExtendedDefaultRules
-- >>> import Data.Monoid
-- >>> import Prelude

{- | A partial semigroup is like a 'Semigroup', but with an operator returning
@'Maybe' a@ rather than @a@.

For comparison:

> (<>)        :: Semigroup a        => a -> a -> a
> appendMaybe :: PartialSemigroup a => a -> a -> Maybe a

-}

class PartialSemigroup a
  where
    appendMaybe :: a -> a -> Maybe a

--------------------------------------------------------------------------------

instance PartialSemigroup () where appendMaybe () () = Just ()

instance PartialSemigroup a => PartialSemigroup (Identity a)
  where
    appendMaybe (Identity x) (Identity y) = Identity <$> appendMaybe x y

instance PartialSemigroup [a] where appendMaybe x y = Just (x <> y)

--------------------------------------------------------------------------------

instance (PartialSemigroup a, PartialSemigroup b) =>
  PartialSemigroup (Either a b)
  where
    appendMaybe (Left x) (Left y) = Left <$> appendMaybe x y
    appendMaybe (Right x) (Right y) = Right <$> appendMaybe x y
    appendMaybe _ _ = Nothing

{- $either

The exemplary nontrivial 'PartialSemigroup' is 'Either', for which the append
operator produces a 'Just' result only if both arguments are 'Left' or both
arguments are 'Right'.

>>> appendMaybe (Left "ab") (Left "cd")
Just (Left "abcd")

>>> appendMaybe (Left "ab") (Right [1,2])
Nothing

-}

--------------------------------------------------------------------------------

{- $tuple

A tuple forms a partial semigroups when all of its constituent parts have
partial semigroups. The append operator returns a 'Just' value when /all/ of the
fields' append operators must return 'Just' values.

>>> x = (Left "ab", Right "hi")
>>> y = (Left "cd", Right "jk")
>>> appendMaybe x y
Just (Left "abcd",Right "hijk")

>>> x = (Left "ab", Right "hi")
>>> y = (Left "cd", Left "jk")
>>> appendMaybe x y
Nothing

-}

instance (PartialSemigroup a, PartialSemigroup b) =>
  PartialSemigroup (a, b)
  where
    appendMaybe (a, b) (a', b') =
      (,) <$> appendMaybe a a' <*> appendMaybe b b'

instance (PartialSemigroup a, PartialSemigroup b, PartialSemigroup c) =>
  PartialSemigroup (a, b, c)
  where
    appendMaybe (a, b, c) (a', b', c') =
      (,,) <$> appendMaybe a a' <*> appendMaybe b b' <*> appendMaybe c c'

--------------------------------------------------------------------------------

{- | Apply a semigroup operation to any pairs of consecutive list elements where
the semigroup operation is defined over them. -}

-- | ==== Examples

-- | For 'Either', 'groupAndConcat' combines contiguous sublists of 'Left' and
-- contiguous sublists of 'Right'.
--
-- >>> xs = [Left "a", Right "b", Right "c", Left "d", Left "e", Left "f"]
-- >>> groupAndConcat xs
-- [Left "a",Right "bc",Left "def"]

groupAndConcat :: forall a. PartialSemigroup a => [a] -> [a]
groupAndConcat =
  \case
    []         -> []
    x : []     -> [x]
    x : y : zs -> case appendMaybe x y of
                    Nothing -> x : groupAndConcat (y : zs)
                    Just a  ->     groupAndConcat (a : zs)

{- | If @xs@ is nonempty and the partial semigroup operator is defined for all
pairs of values in @xs@, then @'partialConcat' xs@ produces a 'Just' result with
the combination of all the values. Otherwise, returns 'Nothing'. -}

-- | ==== Examples

-- | When all values can combine, we get a 'Just' of their combination.
--
-- >>> partialConcat [Left "a", Left "b", Left "c"]
-- Just (Left "abc")

-- | When some values cannot be combined, we get 'Nothing'.
--
-- >>> partialConcat [Left "a", Left "b", Right "c"]
-- Nothing

-- | When the list is empty, we get 'Nothing'.
--
-- >>> partialConcat []
-- Nothing

partialConcat :: forall a. PartialSemigroup a => [a] -> Maybe a
partialConcat x =
  nonEmpty x >>= partialConcat1

{- | Like 'partialConcat', but for non-empty lists. -}

-- | ==== Examples

-- | When all values can combine, we get a 'Just' of their combination.
--
-- >>> partialConcat1 (Left "a" :| [Left "b", Left "c"])
-- Just (Left "abc")

-- | When some values cannot be combined, we get 'Nothing'.
--
-- >>> partialConcat1 (Left "a" :| [Left "b", Right "c"])
-- Nothing

partialConcat1 :: forall a. PartialSemigroup a => NonEmpty a -> Maybe a
partialConcat1 =
  \case
    x :| [] -> Just x
    x :| (y : zs) ->
      do
        a <- appendMaybe x y
        partialConcat1 (a :| zs)

-- | ==== Examples

-- | If lists are the same length and each pair of elements successfully, then
-- we get a 'Just' result.
--
-- >>> xs = [Left "a", Left "b", Right "c"]
-- >>> ys = [Left "1", Left "2", Right "3"]
-- >>> partialZip xs ys
-- Just [Left "a1",Left "b2",Right "c3"]

-- | If the pairs do not all combine, then we get 'Nothing'.
--
-- >>> xs = [Left "a", Left "b", Right "c"]
-- >>> ys = [Left "1", Right "2", Right "3"]
-- >>> partialZip xs ys
-- Nothing

-- | If the lists have different lengths, then we get 'Nothing'.
--
-- >>> xs = [Left "a", Left "b", Right "c"]
-- >>> ys = [Left "1", Left "2"]
-- >>> partialZip xs ys
-- Nothing

partialZip :: forall a. PartialSemigroup a => [a] -> [a] -> Maybe [a]
partialZip [] [] = Just []
partialZip [] _  = Nothing
partialZip _  [] = Nothing
partialZip (x:xs) (y:ys) = (:) <$> appendMaybe x y <*> partialZip xs ys

{- | Like 'partialZip', but for non-empty lists. -}

-- | ==== Examples

-- | If lists are the same length and each pair of elements successfully, then
-- we get a 'Just' result.
--
-- >>> xs = Left "a" :| [Left "b", Right "c"]
-- >>> ys = Left "1" :| [Left "2", Right "3"]
-- >>> partialZip1 xs ys
-- Just (Left "a1" :| [Left "b2",Right "c3"])

-- | If the pairs do not all combine, then we get 'Nothing'.
--
-- >>> xs = Left "a" :| [Left "b", Right "c"]
-- >>> ys = Left "1" :| [Right "2", Right "3"]
-- >>> partialZip1 xs ys
-- Nothing

-- | If the lists have different lengths, then we get 'Nothing'.
--
-- >>> xs = Left "a" :| [Left "b", Right "c"]
-- >>> ys = Left "1" :| [Left "2"]
-- >>> partialZip1 xs ys
-- Nothing

partialZip1 :: forall a. PartialSemigroup a
  => NonEmpty a -> NonEmpty a -> Maybe (NonEmpty a)
partialZip1 (x :| xs) (y :| ys) =
  (:|) <$> appendMaybe x y <*> partialZip xs ys

-- | 'partialZip'

instance PartialSemigroup a => PartialSemigroup (ZipList a)
  where
    appendMaybe (ZipList x) (ZipList y) = ZipList <$> partialZip x y

--------------------------------------------------------------------------------

{- $total

Every type with a 'Semigroup' can be given a trivial 'PartialSemigroup' instance
defined as:

@
'appendMaybe' x y = 'Just' (x <> y)
@

Additionally, any type with a 'Semigroup' can be treated as a 'PartialSemigroup'
by lifting it into 'TotalSemigroup'.

-}

{- | A wrapper to turn any value with a 'Semigroup' instance into a value with a
'PartialSemigroup' instance whose 'appendMaybe' operator always returns 'Just'.
-}

-- | ==== Examples

-- |
-- >>> appendMaybe (TotalSemigroup "ab") (TotalSemigroup "cd")
-- Just (TotalSemigroup {unTotalSemigroup = "abcd"})

-- |
-- >>> f = getProduct . unTotalSemigroup
-- >>> g = TotalSemigroup . Product
-- >>> fmap f . partialConcat . fmap g $ [1..4]
-- Just 24

newtype TotalSemigroup a = TotalSemigroup { unTotalSemigroup :: a }
  deriving Show

instance Semigroup a => PartialSemigroup (TotalSemigroup a)
  where
    appendMaybe (TotalSemigroup x) (TotalSemigroup y) =
      Just (TotalSemigroup (x <> y))

--------------------------------------------------------------------------------

{- | A wrapper for 'Either' where the 'PartialSemigroup' operator is defined
only over 'Left' values. -}

-- | ==== Examples

-- | Two 'Left's make a 'Just'.
--
-- >>> appendMaybe (AppendLeft (Left "ab")) (AppendLeft (Left "cd"))
-- Just (AppendLeft {unAppendLeft = Left "abcd"})

-- | Anything else produces 'Nothing'
--
-- >>> appendMaybe (AppendLeft (Right "ab")) (AppendLeft (Right "cd"))
-- Nothing

-- | 'groupAndConcat' combines consecutive 'Left' values, leaving the 'Right'
-- values unmodified.
--
-- >>> xs = [Left "a", Left "b", Right "c", Right "d", Left "e", Left "f"]
-- >>> fmap unAppendLeft . groupAndConcat . fmap AppendLeft $ xs
-- [Left "ab",Right "c",Right "d",Left "ef"]

newtype AppendLeft a b = AppendLeft { unAppendLeft :: Either a b }
  deriving Show

instance PartialSemigroup a => PartialSemigroup (AppendLeft a b)
  where
    appendMaybe (AppendLeft (Left x)) (AppendLeft (Left y)) =
      AppendLeft . Left <$> appendMaybe x y
    appendMaybe _ _ = Nothing

--------------------------------------------------------------------------------

{- | A wrapper for 'Either' where the 'PartialSemigroup' operator is defined
only over 'Right' values. -}

-- | ==== Examples

-- | Two 'Right's make a 'Just'.
--
-- >>> appendMaybe (AppendRight (Right "ab")) (AppendRight (Right "cd"))
-- Just (AppendRight {unAppendRight = Right "abcd"})

-- | Anything else produces 'Nothing'
--
-- >>> appendMaybe (AppendRight (Left "ab")) (AppendRight (Left "cd"))
-- Nothing

-- | 'groupAndConcat' combines consecutive 'Right' values, leaving the 'Left'
-- values unmodified.
--
-- >>> xs = [Left "a", Left "b", Right "c", Right "d", Left "e", Left "f"]
-- >>> fmap unAppendRight . groupAndConcat . fmap AppendRight $ xs
-- [Left "a",Left "b",Right "cd",Left "e",Left "f"]

newtype AppendRight a b = AppendRight { unAppendRight :: Either a b }
  deriving Show

instance PartialSemigroup b => PartialSemigroup (AppendRight a b)
  where
    appendMaybe (AppendRight (Right x)) (AppendRight (Right y)) =
      AppendRight . Right <$> appendMaybe x y
    appendMaybe _ _ = Nothing
