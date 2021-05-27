import System.Environment
import System.Process

main =
  getEnv "ghc" >>= \ghc ->
    case ghc of
      "8.0.2"  -> callProcess "cabal" ["test", "all"
                  , "--constraint=hedgehog == 0.5"]
      "8.2.2"  -> callProcess "cabal" ["test", "all"]
      "8.4.4"  -> callProcess "cabal" ["test", "all"]
      "8.6.3"  -> callProcess "cabal" ["test", "all"]
      "8.8.1"  -> callProcess "cabal" ["test", "all"]
      "8.10.1" -> callProcess "cabal" ["test", "all"]
      "9.0.1"  -> callProcess "cabal" ["test", "all"
                  , "--constraint=hedgehog == 1.0.5"
                  ]
