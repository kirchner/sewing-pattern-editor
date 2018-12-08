{ pkgs ? import <nixpkgs> {} }:

with pkgs; stdenv.mkDerivation {
  name = "sewing-pattern-editor";

  buildInputs = [
    yarn
  ];
}
