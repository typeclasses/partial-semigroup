{ pkgs ? import <nixpkgs> {}
, ghc-version ? "8.2.1"
}:

let
  inherit (pkgs) haskell cabal-install glibcLocales;
  inherit (pkgs.stdenv) mkDerivation;

  ghc =
    {
      "7.8.4"  = haskell.compiler.ghc784;
      "7.10.3" = haskell.compiler.ghc7103;
      "8.0.2"  = haskell.compiler.ghc802;
      "8.2.1"  = haskell.compiler.ghc821;
    }.${ghc-version};

  echo-header = x: ''
    echo ""
    echo "------------------------------------------------------------"
    echo "  ${x}"
    echo "------------------------------------------------------------"
    echo ""
  '';

in

  mkDerivation {
    name = "partial-semigroup-env";

    buildInputs = [ ghc cabal-install glibcLocales ];

    shellHook = ''
      ci-script () {
        export LC_ALL=en_US.UTF-8
        cabal update

        ${echo-header "Haddock: partial-semigroup"}
        cabal new-haddock partial-semigroup

        ${echo-header "Haddock: partial-semigroup-test"}
        cabal new-haddock partial-semigroup-test

        ${echo-header "Test: partial-semigroup:examples"}
        cabal new-test partial-semigroup:examples

        ${echo-header "Test: partial-semigroup:properties"}
        cabal new-test partial-semigroup:properties
      }
    '';
  }
