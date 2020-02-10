{ pkgs ? (
    import (builtins.fetchTarball {
      name = "nixos-unstable-2020-02-08";
      url = https://github.com/nixos/nixpkgs/archive/338c988c9a1447ea045ee123ba204da0e26ea1c8.tar.gz;
      sha256 = "052z61v96q199ra4z5zi896m0a2bfjz59gidad0z5z8m9hvjzf1p";
    }) {}
  )
}:

with pkgs;

mkShell {
  buildInputs = [
    elmPackages.elm
    elmPackages.elm-test
    elmPackages.elm-format
    elmPackages.elm-doc-preview
    elm2nix
    expect
    cabal2nix
  ];
}
