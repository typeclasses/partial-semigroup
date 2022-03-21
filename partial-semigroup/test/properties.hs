-- partial-semigroup
import Data.PartialSemigroup (AppendLeft (..), AppendRight (..), AtMostOne (..),
                              One (..), Total (..))

-- partial-semigroup-test
import Test.PartialSemigroup.Hedgehog (assoc)

-- hedgehog
import           Hedgehog       (Gen, Property)
import qualified Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

-- base
import           Control.Applicative   (ZipList (..))
import           Control.Monad         (unless)
import           Data.Foldable         (for_)
import           Data.Functor.Identity (Identity (..))
import           Data.Monoid           (Sum (..))
import qualified System.Exit           as Exit
import qualified System.IO             as IO

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

prop_identity_assoc :: Property
prop_identity_assoc =
  assoc (Identity <$> genStr)

prop_list_assoc :: Property
prop_list_assoc =
  assoc genStr

prop_list_total_assoc :: Property
prop_list_total_assoc =
  assoc (Total <$> genStr)

prop_zipList_assoc :: Property
prop_zipList_assoc =
  assoc (ZipList <$> Gen.list (Range.linear 0 3) genEither)

prop_either_assoc :: Property
prop_either_assoc =
  assoc genEither

prop_tuple2_assoc :: Property
prop_tuple2_assoc =
  assoc ((,) <$> genStr <*> genEither)

prop_tuple3_assoc :: Property
prop_tuple3_assoc =
  assoc ((,,) <$> genStr <*> genEither <*> genSum)

prop_appendLeft_assoc :: Property
prop_appendLeft_assoc =
  assoc (AppendLeft <$> genEither)

prop_appendRight_assoc :: Property
prop_appendRight_assoc =
  assoc (AppendRight <$> genEither)

prop_one_assoc :: Property
prop_one_assoc =
  assoc (One <$> genMaybe)

prop_atMostOne_assoc :: Property
prop_atMostOne_assoc =
  assoc (AtMostOne <$> genMaybe)


--------------------------------------------------------------------------------
--  Generators
--------------------------------------------------------------------------------

genStr :: Gen String
genStr =
  Gen.string (Range.linear 0 5) Gen.alpha

genSum :: Gen (Sum Integer)
genSum =
  Sum <$> Gen.integral (Range.linear 0 10)

genMaybe  :: Gen (Maybe String)
genMaybe =
  Gen.maybe genStr

genEither :: Gen (Either String (Sum Integer))
genEither =
  Gen.choice
    [ Left <$> genStr
    , Right <$> genSum
    ]
