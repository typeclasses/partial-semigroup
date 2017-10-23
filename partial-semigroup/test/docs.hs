{-# LANGUAGE CPP #-}

#ifdef DOCTEST

import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "src/Data/PartialSemigroup.hs"
    , "src/Data/PartialSemigroup/Generics.hs"
    ]

#else

main :: IO ()
main = putStrLn "Tests using doctest are disabled."

#endif
