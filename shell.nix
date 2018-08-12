{ pkgs ? import <nixpkgs> {} }:
{
  elm = pkgs.stdenv.mkDerivation rec {
    name = "elm-0.19-alpha-2";
    src = pkgs.fetchurl {
      url = https://44a95588fe4cc47efd96-ec3c2a753a12d2be9f23ba16873acc23.ssl.cf2.rackcdn.com/linux-64.tar.gz;
      sha256 = "0cam9yr7ljshrf8w5mdhjlmlf84fpd5c2aacm4gac124229a75l6";
    };
    libPath = pkgs.stdenv.lib.makeLibraryPath [
        pkgs.zlib pkgs.ncurses5 pkgs.gmp pkgs.libffi
      ];
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      tar xf $src -C $out/bin
      patchelf \
        --set-rpath ${libPath} \
        --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
        $out/bin/elm
    '';
  };
}
