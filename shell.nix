{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  latestPkgs =
    import (builtins.fetchTarball {
      name = "nixos-unstable-2019-10-26";
      url = https://github.com/nixos/nixpkgs/archive/c69ebd2883dfca8621b34e95a4e006b0c34ee7b9.tar.gz;
      sha256 = "0irablnpc13rs652qn4h32zx4z6bqvibj521yazmmccb9kdg9d5v";
    }) {};

  ghc = haskellPackages.ghcWithPackages(pkgs: with pkgs; [
    zlib
  ]);

in

stdenv.mkDerivation {
  name = "sewing-pattern-editor";

  buildInputs = [
    yarn
    latestPkgs.elmPackages.elm
    latestPkgs.elmPackages.elm-test
    latestPkgs.elmPackages.elm-format
    latestPkgs.elmPackages.elm-doc-preview
    expect
    ghc
    haskellPackages.cabal-install
    cabal2nix
  ];

  shellHook = ''
  '';
}
