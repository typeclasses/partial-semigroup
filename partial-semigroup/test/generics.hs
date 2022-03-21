{-# LANGUAGE DeriveGeneric #-}

-- partial-semigroup
import Data.PartialSemigroup          (PartialSemigroup (..))
import Data.PartialSemigroup.Generics (genericPartialSemigroupOp)

-- partial-semigroup-test
import Test.PartialSemigroup.Hedgehog (assoc)

-- hedgehog
import           Hedgehog       (Gen, Property, property, withDiscards,
                                 withTests, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

-- base
import           Control.Monad (unless)
import           Data.Foldable (for_)
import           GHC.Generics  (Generic)
import qualified System.Exit   as Exit
import qualified System.IO     as IO

main :: IO ()
main = do
  for_ [IO.stdout, IO.stderr] $ \h -> do
    IO.hSetEncoding h IO.utf8
    IO.hSetBuffering h IO.LineBuffering
  success <- Hedgehog.checkParallel $$(Hedgehog.discover)
  unless success Exit.exitFailure


--------------------------------------------------------------------------------
--  The type whose partial semigroup instance we'll be testing
--------------------------------------------------------------------------------

data T
  = A String (Either String String)
  | B String
  deriving (Eq, Generic, Show)

instance PartialSemigroup T where
  (<>?) = genericPartialSemigroupOp


--------------------------------------------------------------------------------
--  Generators
--------------------------------------------------------------------------------

genStr :: Gen String
genStr =
  Gen.string (Range.linear 0 5) Gen.alpha

genT :: Gen T
genT =
  Gen.choice
    [ A <$> genStr <*> Gen.choice [ Left <$> genStr, Right <$> genStr ]
    , B <$> genStr
    ]


--------------------------------------------------------------------------------
--  Associative property
--------------------------------------------------------------------------------

prop_assoc :: Property
prop_assoc =
  withDiscards 1000 $
  assoc genT


--------------------------------------------------------------------------------
--  Examples
--------------------------------------------------------------------------------

prop_example_1 :: Property
prop_example_1 = withTests 1 $ property $
  let
    x = A "s" (Left "x")
    y = A "t" (Left "y")
  in
    x <>? y === Just (A "st" (Left "xy"))

prop_example_2 :: Property
prop_example_2 = withTests 1 $ property $
  let
    x = B "x"
    y = B "y"
  in
    x <>? y === Just (B "xy")

prop_example_3 :: Property
prop_example_3 = withTests 1 $ property $
  let
    x = A "s" (Left "x")
    y = A "t" (Right "y")
  in
    x <>? y === Nothing

prop_example_4 :: Property
prop_example_4 = withTests 1 $ property $
  let
    x = A "x" (Left "y")
    y = B "z"
  in
    x <>? y === Nothing
