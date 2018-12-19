{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let

  elmiToJsonNixpkgs = fetchgit {
    url = "https://github.com/mdevlamynck/nixpkgs";
    sha256 = "1wmf09igcmdrizbv1j15irchxv72w4cm33dsxc6y1z4l3fpya4i7";
    rev = "4746b5b7d26567b45364a5864af03e7ad8e91f0a";
  };

  elmiToJson = (import elmiToJsonNixpkgs {}).pkgs.elmPackages.elmi-to-json;

in

stdenv.mkDerivation {
  name = "sewing-pattern-editor";

  buildInputs = [
    yarn
  ];

  shellHook = ''
    rm ./node_modules/elmi-to-json/unpacked_bin/elmi-to-json
    ln -s ${elmiToJson}/bin/elmi-to-json ./node_modules/elmi-to-json/unpacked_bin/
  '';
}
