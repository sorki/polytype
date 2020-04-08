{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.haskellPackages.callCabal2nix "polytype" ./. { }
#nixpkgs.haskell.packages.ghc883.callCabal2nix "polytype" ./. { }
