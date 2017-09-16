{-# LANGUAGE TemplateHaskell #-}

-- Hedgehog
import           Hedgehog       (Property, property, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

-- Base
import           Control.Monad (unless)
import           Data.Foldable (for_)
import qualified System.Exit   as Exit
import qualified System.IO     as IO

main :: IO ()
main = do
  for_ [IO.stdout, IO.stderr] $ \h -> do
    IO.hSetEncoding h IO.utf8
    IO.hSetBuffering h IO.LineBuffering
  success <- Hedgehog.checkParallel $$(Hedgehog.discover)
  unless success Exit.exitFailure
