A partial semigroup is like a semigroup, but the operator is partial.
We represent this in Haskell as a total function:

```haskell
(<>?) :: a -> a -> Maybe a
```

The [partial-semigroup-hedgehog] companion package provides support for checking
the partial semigroup associativity axiom using the [hedgehog] package.

## Semigroups (background)

A *semigroup* is a set with a binary associative operator. In Haskell we
represent semigroups as instances of the `Semigroup` typeclass, which looks
something like this:

```haskell
class Semigroup a where (<>) :: a -> a -> a
```

This was once provided by the [semigroups] package, but is now in the Haskell
standard library as of `base 4.9.0.0` in 2016.

### The semigroup associativity axiom

The semigroup *associativity* axiom is stated as:

```haskell
(a <> b) <> c = a <> (b <> c)
```

## Partial semigroups

A *partial semigroup* can be defined in two equivalent ways:

  1. As a semigroup where `<>` is a *partial function* (that is, we admit the
     possibility that `x <> y = ⊥` for some `x` and `y`)
  2. As a new kind of algebraic structure where the operation is *total* (not
     partial) but returns `Maybe a` instead of `a`.

The second definition is the approach we take here (though we will refer back to
this first definition when we discuss the associativity axiom). The
`partial-semigroup` package defines the `PartialSemigroup` class, which looks
like this:

```haskell
class PartialSemigroup a where (<>?) :: a -> a -> Maybe a
```

### The partial semigroup associativity axiom

The partial semigroup associativity axiom is a natural adaptation of the
semigroup associativity axiom, with a slight modification to accommodate
the situations wherein `x <> y = ⊥`. First we'll express the axiom in terms
of `Semigroup` and `⊥`, and then we'll rephrase it in terms of
`PartialSemigroup`.

#### Definition 1: In terms of `Semigroup` and `⊥`

For all `x`, `y`, `z`:

  * If `x <> y ≠ ⊥` and `y <> z ≠ ⊥`, then

      * `x <> (y <> z) = ⊥` if and only if `(x <> y) <> z = ⊥`, and

      * where none of the terms are ⊥, the axiom for total semigroups
        `x <> (y <> z) = (x <> y) <> z` must hold.

#### Definition 2: In terms of `PartialSemigroup`

For all `x`, `y`, `z`:

  * If `x <>? y = Just xy` and `y <>? z = Just yz`, then

      * `x <>? yz = xy <>? z`.

## Deriving using GHC generics

If a type derives `Generic` and all of its fields have `PartialSemigroup`
instances, you can get a `PartialSemigroup` for free.

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.PartialSemigroup.Generics

data T
  = A String (Either String String)
  | B String
  deriving (Eq, Generic, Show)

instance PartialSemigroup T where
  (<>?) = genericPartialSemigroupOp
```

This gives us an implementation of `<>?` which combines values only if they have
the same structure.

```haskell
λ> A "s" (Left "x") <>? A "t" (Left "y")
Just (A "st" (Left "xy"))

>>> B "x" <>? B "y"
Just (B "xy")
```

For values that do *not* have the same structure, `<>?` produces `Nothing`.

```haskell
>>> A "s" (Left "x") <>? A "t" (Right "y")
Nothing

>>> A "x" (Left "y") <>? B "z"
Nothing
```

  [partial-semigroup-hedgehog]: https://hackage.haskell.org/package/partial-semigroup-hedgehog

  [hedgehog]: https://hackage.haskell.org/package/hedgehog

  [semigroups]: https://hackage.haskell.org/package/semigroups
