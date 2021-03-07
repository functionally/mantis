{
  pkgs ? import cardano-node/nix/default.nix {}
}:

let

  haskell = pkgs.haskell.packages.ghc8102;

  haskellPackages = haskell.override {
    overrides = self: super: pkgs.cardanoNodeHaskellPackages;
  };

  ghc = haskellPackages.ghcWithHoogle (ps: with ps; [
    cardano-api
    cardano-cli
    cardano-node
  ]);

in

  pkgs.stdenv.mkDerivation {
    name = "cardano-env";
    buildInputs = [
      ghc
      haskell.cabal-install
      haskell.ghcide
    # haskell.hdevtools
      haskell.hlint
      haskell.pointfree
    ];
  }
