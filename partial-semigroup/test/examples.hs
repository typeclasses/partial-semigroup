{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell      #-}

#ifdef HEDGEHOG

-- partial-semigroup
import Data.PartialSemigroup (AppendLeft (..), AppendRight (..),
                              PartialSemigroup ((<>?)), Total (..),
                              groupAndConcat, partialConcat, partialConcat1,
                              partialZip, partialZip1)

-- hedgehog
import           Hedgehog (Property, PropertyT, property, withTests, (===))
import qualified Hedgehog

-- base
import           Control.Monad      (unless)
import           Data.Foldable      (for_)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Monoid        (Product (..))
import qualified System.Exit        as Exit
import qualified System.IO          as IO

main :: IO ()
main = do
  for_ [IO.stdout, IO.stderr] $ \h -> do
    IO.hSetEncoding h IO.utf8
    IO.hSetBuffering h IO.LineBuffering
  success <- Hedgehog.checkParallel $$(Hedgehog.discover)
  unless success Exit.exitFailure

example :: PropertyT IO () -> Property
example = withTests 1 . property


--------------------------------------------------------------------------------
--  Examples
--------------------------------------------------------------------------------

prop_Left_Left :: Property
prop_Left_Left = example $
  Left "ab" <>? Left "cd" === Just (Left "abcd")

prop_Left_Right :: Property
prop_Left_Right = example $
  Left "ab" <>? Right [1, 2] === Nothing

prop_tuple_Just :: Property
prop_tuple_Just = example $
  let
    x = (Left "ab", Right "hi")
    y = (Left "cd", Right "jk")
  in
    x <>? y === Just (Left "abcd", Right "hijk")

prop_tuple_Nothing :: Property
prop_tuple_Nothing = example $
  let
    x = (Left "ab", Right "hi")
    y = (Left "cd", Left "jk")
  in
    x <>? y === Nothing

prop_groupAndConcat :: Property
prop_groupAndConcat = example $
  let
    xs = [Left "a", Right "b", Right "c", Left "d", Left "e", Left "f"]
  in
    groupAndConcat xs === [Left "a", Right "bc", Left "def"]

prop_partialConcat_Just :: Property
prop_partialConcat_Just = example $
  partialConcat [Left "a", Left "b", Left "c"] === Just (Left "abc")

prop_partialConcat_item'mismatch :: Property
prop_partialConcat_item'mismatch = example $
  partialConcat [Left "a", Left "b", Right "c"] === Nothing

prop_partialConcat_empty :: Property
prop_partialConcat_empty = example $
  partialConcat [] === Nothing

prop_partialConcat1_Just :: Property
prop_partialConcat1_Just = example $
  partialConcat1 (Left "a" :| [Left "b", Left "c"]) === Just (Left "abc")

prop_partialConcat1_item'mismatch :: Property
prop_partialConcat1_item'mismatch = example $
  partialConcat1 (Left "a" :| [Left "b", Right "c"]) === Nothing

prop_partialZip_Just :: Property
prop_partialZip_Just = example $
  let
    xs = [Left "a", Left "b", Right "c"]
    ys = [Left "1", Left "2", Right "3"]
  in
    partialZip xs ys === Just [Left "a1", Left "b2", Right "c3"]

prop_partialZip_item'mismatch :: Property
prop_partialZip_item'mismatch = example $
  let
    xs = [Left "a", Left "b", Right "c"]
    ys = [Left "1", Right "2", Right "3"]
  in
    partialZip xs ys === Nothing

prop_partialZip_empty :: Property
prop_partialZip_empty = example $
  let
    xs = [Left "a", Left "b", Right "c"]
    ys = [Left "1", Left "2"]
  in
    partialZip xs ys === Nothing

prop_partialZip1_Just :: Property
prop_partialZip1_Just = example $
  let
    xs = Left "a" :| [Left "b", Right "c"]
    ys = Left "1" :| [Left "2", Right "3"]
  in
    partialZip1 xs ys === Just (Left "a1" :| [Left "b2", Right "c3"])

prop_partialZip1_item'mismatch :: Property
prop_partialZip1_item'mismatch = example $
  let
    xs = Left "a" :| [Left "b", Right "c"]
    ys = Left "1" :| [Right "2", Right "3"]
  in
    partialZip1 xs ys === Nothing

prop_partialZip1_length'mismatch :: Property
prop_partialZip1_length'mismatch = example $
  let
    xs = Left "a" :| [Left "b", Right "c"]
    ys = Left "1" :| [Left "2"]
  in
    partialZip1 xs ys === Nothing

prop_Total_Just :: Property
prop_Total_Just = example $
  Total "ab" <>? Total "cd" === Just (Total "abcd")

prop_Total_partialConcat :: Property
prop_Total_partialConcat = example $
  let
    f = getProduct . unTotal
    g = Total . Product
  in
    (fmap f . partialConcat . fmap g) [1..4] === Just 24

prop_AppendLeft_Just :: Property
prop_AppendLeft_Just = example $
  let
    x = AppendLeft (Left "ab")
    y = AppendLeft (Left "cd")
  in
    x <>? y === Just (AppendLeft (Left "abcd"))

prop_AppendLeft_Nothing :: Property
prop_AppendLeft_Nothing = example $
  let
    x = AppendLeft (Right "ab")
    y = AppendLeft (Right "cd")
  in
    x <>? y === Nothing

prop_AppendLeft_groupAndConcat :: Property
prop_AppendLeft_groupAndConcat = example $
  let
    xs = [Left "a", Left "b", Right "c", Right "d", Left "e", Left "f"]
    f = fmap unAppendLeft . groupAndConcat . fmap AppendLeft
  in
    f xs === [Left "ab", Right "c", Right "d", Left "ef"]

prop_AppendRight_Just :: Property
prop_AppendRight_Just = example $
  let
    x = AppendRight (Right "ab")
    y = AppendRight (Right "cd")
  in
    x <>? y === Just (AppendRight (Right "abcd"))

prop_AppendRight_Nothing :: Property
prop_AppendRight_Nothing = example $
  let
    x = AppendRight (Left "ab")
    y = AppendRight (Left "cd")
  in
    x <>? y === Nothing

prop_AppendRight_groupAndConcat :: Property
prop_AppendRight_groupAndConcat = example $
  let
    xs = [Left "a", Left "b", Right "c", Right "d", Left "e", Left "f"]
    f = fmap unAppendRight . groupAndConcat . fmap AppendRight
  in
    f xs === [Left "a", Left "b", Right "cd", Left "e", Left "f"]

#else

main :: IO ()
main = putStrLn "Tests using hedgehog are disabled."

#endif
