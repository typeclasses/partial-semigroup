-- | A /semigroup/ ('Semigroup') is a set with a binary associative operation (@<>@).
--
-- This module defines a /partial semigroup/ ('PartialSemigroup'), a
-- semigroup for which @<>@ is not required to be defined over all inputs.
module Data.PartialSemigroup
  ( -- * Partial semigroup
    PartialSemigroup (..),

    -- * Partial monoid
    PartialMonoid (..),
    pmappend,

    -- * Either
    -- $either
    AppendLeft (..),
    AppendRight (..),

    -- * Tuples
    -- $tuple

    -- * Concatenation
    groupAndConcat,
    partialConcat,
    partialConcat1,

    -- * Zipping
    partialZip,
    partialZip1,

    -- * Total to partial
    -- $total
    Total (..),

    -- * Partial to total
    -- $partial
    Partial (..),

    -- * Refusing to combine
    -- $refusing
    One (..),
    AtMostOne (..),
  )
where

import Control.Applicative (ZipList (..), (<$>), (<*>))
import Control.Monad (foldM, (>>=))
import Data.Either (Either (..))
import Data.Function (($), (.))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (fromJust, Maybe (..))
import Data.Monoid (Monoid (..), Product (..), Sum (..))
import Data.Semigroup (Semigroup (..))
import Prelude (Eq, Num (..), Ord, Read, Show)

-- $setup
--
-- >>> import Data.Function (($))
-- >>> import Data.Functor (fmap)

-- The same fixity as <>
infixr 6 <>?

-- | A 'PartialSemigroup' is like a 'Semigroup', but with an operator returning
-- @'Maybe' a@ rather than @a@.
--
-- For comparison:
--
-- @
-- ('<>')  :: 'Semigroup' a        => a -> a -> a
-- ('<>?') :: 'PartialSemigroup' a => a -> a -> 'Maybe' a
-- @
--
-- === The associativity axiom for partial semigroups
--
-- For all @x@, @y@, @z@:
--
--  * If @x '<>?' y = 'Just' xy@ and @y '<>?' z = 'Just' yz@, then
--
--      * @x '<>?' yz = xy '<>?' z@.
--
-- ==== Relationship to the semigroup associativity axiom
--
-- The partial semigroup associativity axiom is a natural adaptation of the
-- semigroup associativity axiom
--
-- @x '<>' (y '<>' z) = (x '<>' y) '<>' z@
--
-- with a slight modification to accommodate situations where '<>' is undefined. We
-- may gain some insight into the connection between 'Semigroup' and
-- 'PartialSemigroup' by rephrasing the partial semigroup associativity in terms of
-- a partial '<>' operator thusly:
--
-- For all @x@, @y@, @z@:
--
--  * If @x '<>' y@ and @y '<>' z@ are both defined, then
--
--      * @x '<>' (y '<>' z)@ is defined if and only if @(x '<>' y) '<>' z@ is
--        defined, and
--
--      * if these things /are/ all defined, then the axiom for total semigroups
--        @x '<>' (y '<>' z) = (x '<>' y) '<>' z@ must hold.
class PartialSemigroup a where
  (<>?) :: a -> a -> Maybe a

--------------------------------------------------------------------------------

-- | A 'PartialMonoid' is like a 'Monoid', but with an operator returning
-- @'Maybe' a@ rather than @a@.  Every 'PartialMonoid' is a 'PartialSemigroup'.
--
-- == The identity axioms for partial monoids
--
-- For all @x@:
--
--  * @'pmempty' '<>?' x = x '<>?' 'pempty'@.
--
--  * @'pmempty' '<>?' x = 'Nothing'@ or @'pmempty' '<>?' x = 'Just' x@.
--
-- @since 0.7.0.0
class PartialSemigroup a => PartialMonoid a where
  -- | Identity of '<>?'.
  pmempty :: a
  pmempty = fromJust . pmconcat $ []
  {-# INLINE pmempty #-}

  -- | Fold a list using the monoid.
  --
  -- For most types, the default definition of 'pmconcat' will be used, but the
  -- function is included in the class definition so that an optimized version
  -- can be provided for specific types.
  pmconcat :: [a] -> Maybe a
  pmconcat = foldM pmappend pmempty
  {-# INLINE pmconcat #-}

  {-# MINIMAL pmempty | pmconcat #-}

-- | An associative operation.
--
-- This is an alias for '<>?', for compatibility with 'mappend'.
--
-- @since 0.7.0.0
pmappend :: PartialMonoid a => a -> a -> Maybe a
pmappend = (<>?)
{-# INLINE pmappend #-}

--------------------------------------------------------------------------------

instance PartialSemigroup () where
  () <>? () = Just ()

-- | @since 0.7.0.0
instance PartialMonoid () where
  pmempty = ()
  pmconcat _ = Just ()

--------------------------------------------------------------------------------

instance PartialSemigroup [a] where
  x <>? y = Just (x <> y)

-- | @since 0.7.0.0
instance PartialMonoid [a] where
  pmempty = mempty
  pmconcat = Just . mconcat

--------------------------------------------------------------------------------

instance Num a => PartialSemigroup (Sum a) where
  x <>? y = Just (x <> y)

-- | @since 0.7.0.0
instance Num a => PartialMonoid (Sum a) where
  pmempty = mempty
  pmconcat = Just . mconcat

instance Num a => PartialSemigroup (Product a) where
  x <>? y = Just (x <> y)

-- | @since 0.7.0.0
instance Num a => PartialMonoid (Product a) where
  pmempty = mempty
  pmconcat = Just . mconcat

--------------------------------------------------------------------------------

instance PartialSemigroup a => PartialSemigroup (Identity a) where
  Identity x <>? Identity y = Identity <$> (x <>? y)

-- | @since 0.7.0.0
instance PartialMonoid a => PartialMonoid (Identity a) where
  pmempty = Identity pmempty
  pmconcat = pmconcat

--------------------------------------------------------------------------------

instance
  (PartialSemigroup a, PartialSemigroup b) =>
  PartialSemigroup (Either a b)
  where
  Left x <>? Left y = Left <$> (x <>? y)
  Right x <>? Right y = Right <$> (x <>? y)
  _ <>? _ = Nothing

-- $either
--
-- The exemplary nontrivial 'PartialSemigroup' is 'Either', for which the append
-- operator produces a 'Just' result only if both arguments are 'Left' or both
-- arguments are 'Right'.
--
-- >>> Left "ab" <>? Left "cd"
-- Just (Left "abcd")
--
-- >>> Left "ab" <>? Right [1, 2]
-- Nothing

--------------------------------------------------------------------------------

-- $tuple
--
-- A tuple forms a partial semigroups when all of its constituent parts have
-- partial semigroups. The append operator returns a 'Just' value when /all/ of the
-- fields' append operators must return 'Just' values.
--
-- >>> x = (Left "ab", Right "hi")
-- >>> y = (Left "cd", Right "jk")
-- >>> x <>? y
-- Just (Left "abcd",Right "hijk")
--
-- >>> x = (Left "ab", Right "hi")
-- >>> y = (Left "cd", Left "jk")
-- >>> x <>? y
-- Nothing

instance (PartialSemigroup a, PartialSemigroup b) => PartialSemigroup (a, b) where
  (a, b) <>? (a', b') =
    (,)
      <$> (a <>? a')
      <*> (b <>? b')

-- | @since 0.7.0.0
instance (PartialMonoid a, PartialMonoid b) => PartialMonoid (a, b) where
  pmempty = (pmempty, pmempty)

instance
  (PartialSemigroup a, PartialSemigroup b, PartialSemigroup c) =>
  PartialSemigroup (a, b, c)
  where
  (a, b, c) <>? (a', b', c') =
    (,,)
      <$> (a <>? a')
      <*> (b <>? b')
      <*> (c <>? c')

-- | @since 0.7.0.0
instance
  (PartialMonoid a, PartialMonoid b, PartialMonoid c) =>
  PartialMonoid (a, b, c)
  where
  pmempty = (pmempty, pmempty, pmempty)

--------------------------------------------------------------------------------

-- | Apply a semigroup operation to any pairs of consecutive list elements where
-- the semigroup operation is defined over them.
--
-- ==== Examples
--
-- For 'Either', 'groupAndConcat' combines contiguous sublists of 'Left' and
-- contiguous sublists of 'Right'.
--
-- >>> xs = [Left "a", Right "b", Right "c", Left "d", Left "e", Left "f"]
-- >>> groupAndConcat xs
-- [Left "a",Right "bc",Left "def"]
groupAndConcat :: PartialSemigroup a => [a] -> [a]
groupAndConcat [] = []
groupAndConcat [x] = [x]
groupAndConcat (x : y : zs) =
  case x <>? y of
    Nothing -> x : groupAndConcat (y : zs)
    Just a -> groupAndConcat (a : zs)

-- | If @xs@ is nonempty and the partial semigroup operator is defined for all
-- pairs of values in @xs@, then @'partialConcat' xs@ produces a 'Just' result with
-- the combination of all the values. Otherwise, returns 'Nothing'.
--
-- ==== Examples
--
-- When all values can combine, we get a 'Just' of their combination.
--
-- >>> partialConcat [Left "a", Left "b", Left "c"]
-- Just (Left "abc")
--
-- When some values cannot be combined, we get 'Nothing'.
--
-- >>> partialConcat [Left "a", Left "b", Right "c"]
-- Nothing
--
-- When the list is empty, we get 'Nothing'.
--
-- >>> partialConcat []
-- Nothing
partialConcat :: PartialSemigroup a => [a] -> Maybe a
partialConcat x =
  nonEmpty x >>= partialConcat1

-- | Like 'partialConcat', but for non-empty lists.
--
-- ==== Examples
--
-- When all values can combine, we get a 'Just' of their combination.
--
-- >>> partialConcat1 (Left "a" :| [Left "b", Left "c"])
-- Just (Left "abc")
--
-- When some values cannot be combined, we get 'Nothing'.
--
-- >>> partialConcat1 (Left "a" :| [Left "b", Right "c"])
-- Nothing
partialConcat1 :: PartialSemigroup a => NonEmpty a -> Maybe a
partialConcat1 (x :| []) = Just x
partialConcat1 (x :| (y : zs)) =
  do
    a <- x <>? y
    partialConcat1 (a :| zs)

-- | ==== Examples
--
-- If lists are the same length and each pair of elements successfully, then we get
-- a 'Just' result.
--
-- >>> xs = [Left "a", Left "b", Right "c"]
-- >>> ys = [Left "1", Left "2", Right "3"]
-- >>> partialZip xs ys
-- Just [Left "a1",Left "b2",Right "c3"]
--
-- If the pairs do not all combine, then we get 'Nothing'.
--
-- >>> xs = [Left "a", Left "b", Right "c"]
-- >>> ys = [Left "1", Right "2", Right "3"]
-- >>> partialZip xs ys
-- Nothing
--
-- If the lists have different lengths, then we get 'Nothing'.
--
-- >>> xs = [Left "a", Left "b", Right "c"]
-- >>> ys = [Left "1", Left "2"]
-- >>> partialZip xs ys
-- Nothing
partialZip :: PartialSemigroup a => [a] -> [a] -> Maybe [a]
partialZip [] [] = Just []
partialZip [] _ = Nothing
partialZip _ [] = Nothing
partialZip (x : xs) (y : ys) =
  (:) <$> (x <>? y) <*> partialZip xs ys

-- | Like 'partialZip', but for non-empty lists.
--
-- ==== Examples
--
-- If lists are the same length and each pair of elements successfully, then we get
-- a 'Just' result.
--
-- >>> xs = Left "a" :| [Left "b", Right "c"]
-- >>> ys = Left "1" :| [Left "2", Right "3"]
-- >>> partialZip1 xs ys
-- Just (Left "a1" :| [Left "b2",Right "c3"])
--
-- If the pairs do not all combine, then we get 'Nothing'.
--
-- >>> xs = Left "a" :| [Left "b", Right "c"]
-- >>> ys = Left "1" :| [Right "2", Right "3"]
-- >>> partialZip1 xs ys
-- Nothing
--
-- If the lists have different lengths, then we get 'Nothing'.
--
-- >>> xs = Left "a" :| [Left "b", Right "c"]
-- >>> ys = Left "1" :| [Left "2"]
-- >>> partialZip1 xs ys
-- Nothing
partialZip1 ::
  PartialSemigroup a =>
  NonEmpty a ->
  NonEmpty a ->
  Maybe (NonEmpty a)
partialZip1 (x :| xs) (y :| ys) =
  (:|) <$> (x <>? y) <*> partialZip xs ys

-- | 'partialZip'
instance PartialSemigroup a => PartialSemigroup (ZipList a) where
  ZipList x <>? ZipList y = ZipList <$> partialZip x y

--------------------------------------------------------------------------------

-- $partial
--
-- For every type @a@ with a 'PartialSemigroup', we can construct a total
-- 'Semigroup' for @'Maybe' a@ as:
--
-- @
-- 'Just' x <> 'Just' y = x '<>?' y
-- _ '<>' _ = 'Nothing'
-- @
--
-- We don't actually define this instance for 'Maybe' because it already has a
-- different 'Semigroup' defined over it, but we do provide the 'Partial' wrapper
-- which has this instance.

-- | A wrapper for 'Maybe' with an error-propagating 'Semigroup'.
newtype Partial a = Partial {unPartial :: Maybe a}
  deriving (Eq, Ord, Read, Show)

instance PartialSemigroup a => Semigroup (Partial a) where
  Partial (Just x) <> Partial (Just y) = Partial (x <>? y)
  _ <> _ = Partial Nothing

-- | @since 0.7.0.0
instance PartialMonoid a => Monoid (Partial a) where
  mempty = Partial . Just $ pmempty

--------------------------------------------------------------------------------

-- $total
--
-- For every type with a 'Semigroup', we can trivially construct a
-- 'PartialSemigroup' as:
--
-- @
-- x '<>?' y = 'Just' (x '<>' y)
-- @
--
-- Additionally, any type with a 'Semigroup' can be treated as a 'PartialSemigroup'
-- by lifting it into 'Total'.

-- | A wrapper to turn any value with a 'Semigroup' instance into a value with a
-- 'PartialSemigroup' instance whose '<>?' operator always returns 'Just'.
--
-- ==== Examples
--
-- >>> Total "ab" <>? Total "cd"
-- Just (Total {unTotal = "abcd"})
--
-- >>> f = getProduct . unTotal
-- >>> g = Total . Product
-- >>> fmap f . partialConcat . fmap g $ [1..4]
-- Just 24
newtype Total a = Total {unTotal :: a}
  deriving (Eq, Ord, Read, Show)

instance Semigroup a => PartialSemigroup (Total a) where
  Total x <>? Total y = Just (Total (x <> y))

-- | @since 0.7.0.0
instance Monoid a => PartialMonoid (Total a) where
  pmempty = Total mempty

--------------------------------------------------------------------------------

-- | A wrapper for 'Either' where the 'PartialSemigroup' operator is defined
-- only over 'Left' values.
--
-- ==== Examples
--
-- Two 'Left's make a 'Just'.
--
-- >>> AppendLeft (Left "ab") <>? AppendLeft (Left "cd")
-- Just (AppendLeft {unAppendLeft = Left "abcd"})
--
-- Anything else produces 'Nothing'
--
-- >>> AppendLeft (Right "ab") <>? AppendLeft (Right "cd")
-- Nothing
--
-- 'groupAndConcat' combines consecutive 'Left' values, leaving the 'Right' values
-- unmodified.
--
-- >>> xs = [Left "a", Left "b", Right "c", Right "d", Left "e", Left "f"]
-- >>> fmap unAppendLeft . groupAndConcat . fmap AppendLeft $ xs
-- [Left "ab",Right "c",Right "d",Left "ef"]
newtype AppendLeft a b = AppendLeft {unAppendLeft :: Either a b}
  deriving (Eq, Ord, Read, Show)

instance PartialSemigroup a => PartialSemigroup (AppendLeft a b) where
  AppendLeft (Left x) <>? AppendLeft (Left y) =
    AppendLeft . Left <$> (x <>? y)
  _ <>? _ = Nothing

-- | @since 0.7.0.0
instance PartialMonoid a => PartialMonoid (AppendLeft a b) where
  pmempty = AppendLeft . Left $ pmempty

--------------------------------------------------------------------------------

-- | A wrapper for 'Either' where the 'PartialSemigroup' operator is defined
-- only over 'Right' values.
--
-- ==== Examples
--
-- Two 'Right's make a 'Just'.
--
-- >>> AppendRight (Right "ab") <>? AppendRight (Right "cd")
-- Just (AppendRight {unAppendRight = Right "abcd"})
--
-- Anything else produces 'Nothing'
--
-- >>> AppendRight (Left "ab") <>? AppendRight (Left "cd")
-- Nothing
--
-- 'groupAndConcat' combines consecutive 'Right' values, leaving the 'Left' values
-- unmodified.
--
-- >>> xs = [Left "a", Left "b", Right "c", Right "d", Left "e", Left "f"]
-- >>> fmap unAppendRight . groupAndConcat . fmap AppendRight $ xs
-- [Left "a",Left "b",Right "cd",Left "e",Left "f"]
newtype AppendRight a b = AppendRight {unAppendRight :: Either a b}
  deriving (Eq, Ord, Read, Show)

instance PartialSemigroup b => PartialSemigroup (AppendRight a b) where
  AppendRight (Right x) <>? AppendRight (Right y) =
    AppendRight . Right <$> (x <>? y)
  _ <>? _ = Nothing

-- | @since 0.7.0.0
instance PartialMonoid b => PartialMonoid (AppendRight a b) where
  pmempty = AppendRight . Right $ pmempty

--------------------------------------------------------------------------------

-- $refusing
--
-- These are 'PartialSemigroup' instances that don't really combine their values
-- at all; whenever more than one thing is present, '<>?' fails.

-- | A partial semigroup operation which always fails.
newtype One a = One {theOne :: a}
  deriving (Eq, Ord, Read, Show)

instance PartialSemigroup (One a) where
  _ <>? _ = Nothing

-- | A wrapper for 'Maybe' whose partial semigroup operation fails when two
-- 'Just's are combined.
newtype AtMostOne a = AtMostOne {theOneMaybe :: Maybe a}
  deriving (Eq, Ord, Read, Show)

instance PartialSemigroup (AtMostOne a) where
  AtMostOne Nothing <>? x = Just x
  x <>? AtMostOne Nothing = Just x
  _ <>? _ = Nothing

-- | @since 0.7.0.0
instance PartialMonoid (AtMostOne a) where
  pmempty = AtMostOne Nothing
