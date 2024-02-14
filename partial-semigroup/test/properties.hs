import Control.Applicative (ZipList (..))
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Sum (..))
import Data.PartialSemigroup
  ( AppendLeft (..),
    AppendRight (..),
    AtMostOne (..),
    One (..),
    Total (..),
  )
import Hedgehog (Gen, Property)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Exit qualified as Exit
import System.IO qualified as IO
import Test.PartialSemigroup.Hedgehog (assoc, identity)

main :: IO ()
main = do
  for_ [IO.stdout, IO.stderr] $ \h -> do
    IO.hSetEncoding h IO.utf8
    IO.hSetBuffering h IO.LineBuffering
  success <- Hedgehog.checkParallel $$(Hedgehog.discover)
  unless success Exit.exitFailure

--------------------------------------------------------------------------------
--  Properties
--------------------------------------------------------------------------------

prop_unit_assoc :: Property
prop_unit_assoc =
  assoc (Gen.constant ())

prop_unit_identity :: Property
prop_unit_identity =
  identity (Gen.constant ())

prop_identity_assoc :: Property
prop_identity_assoc =
  assoc (Identity <$> genStr)

prop_identity_identity :: Property
prop_identity_identity =
  identity (Identity <$> genStr)

prop_list_assoc :: Property
prop_list_assoc =
  assoc genStr

prop_list_identity :: Property
prop_list_identity =
  identity genStr

prop_list_total_assoc :: Property
prop_list_total_assoc =
  assoc (Total <$> genStr)

prop_list_total_identity :: Property
prop_list_total_identity =
  identity (Total <$> genStr)

prop_zipList_assoc :: Property
prop_zipList_assoc =
  assoc (ZipList <$> Gen.list (Range.linear 0 3) genEither)

prop_either_assoc :: Property
prop_either_assoc =
  assoc genEither

prop_tuple2_assoc :: Property
prop_tuple2_assoc =
  assoc ((,) <$> genStr <*> genEither)

prop_tuple2_identity :: Property
prop_tuple2_identity =
  identity ((,) <$> genStr <*> Gen.constant ())

prop_tuple3_assoc :: Property
prop_tuple3_assoc =
  assoc ((,,) <$> genStr <*> genEither <*> genSum)

prop_tuple3_identity :: Property
prop_tuple3_identity =
  identity ((,,) <$> genStr <*> Gen.constant () <*> genSum)

prop_appendLeft_assoc :: Property
prop_appendLeft_assoc =
  assoc (AppendLeft <$> genEither)

prop_appendLeft_identity :: Property
prop_appendLeft_identity =
  identity (AppendLeft <$> genEither)

prop_appendRight_assoc :: Property
prop_appendRight_assoc =
  assoc (AppendRight <$> genEither)

prop_appendRight_identity :: Property
prop_appendRight_identity =
  identity (AppendRight <$> genEither)

prop_one_assoc :: Property
prop_one_assoc =
  assoc (One <$> genMaybe)


prop_atMostOne_assoc :: Property
prop_atMostOne_assoc =
  assoc (AtMostOne <$> genMaybe)

prop_atMostOne_identity :: Property
prop_atMostOne_identity =
  identity (AtMostOne <$> genMaybe)

--------------------------------------------------------------------------------
--  Generators
--------------------------------------------------------------------------------

genStr :: Gen String
genStr =
  Gen.string (Range.linear 0 5) Gen.alpha

genSum :: Gen (Sum Integer)
genSum =
  Sum <$> Gen.integral (Range.linear 0 10)

genMaybe :: Gen (Maybe String)
genMaybe =
  Gen.maybe genStr

genEither :: Gen (Either String (Sum Integer))
genEither =
  Gen.choice
    [ Left <$> genStr,
      Right <$> genSum
    ]
