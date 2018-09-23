{ pkgs ? import <nixpkgs> {} }:
{
  elm = pkgs.stdenv.mkDerivation rec {
    name = "elm-0.19";
    src = pkgs.fetchurl {
      url = https://44a95588fe4cc47efd96-ec3c2a753a12d2be9f23ba16873acc23.ssl.cf2.rackcdn.com/binaries-for-linux-64.tar.gz;
      sha256 = "0f817q36ffz8gxwn76606dll539ggc730zs10sc0v5jm97rvp0ks";
    };
    libPath = pkgs.stdenv.lib.makeLibraryPath [
        pkgs.zlib pkgs.ncurses5 pkgs.gmp pkgs.libffi
      ];
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      tar xf $src -C $out/bin
    '';
  };
}
