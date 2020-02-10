{ mkDerivation
, fetchzip
}:

let

  fontawesome = mkDerivation rec {
    name = "fontawesome";
    version = "5.12.1";
    src = fetchzip {
      stripRoot = true;
      url = "https://use.fontawesome.com/releases/v${version}/fontawesome-free-${version}-web.zip";
      sha256 = "0xizbax7a256rs4wsswcxfkgc33nij11xlmmzpjs14bpja48alid";
    };

    installPhase = ''
      mkdir -p $out/share
      cp -R $src/fontawesome-free-${version}-web/* $out/share
    '';
  };

in

mkDerivation {
  name = "assets";
  src = ./.;

  dontBuild = true;

  installPhase = ''
    mkdir $out

    cp \
      $src/service-worker.js \
      $src/register-service-worker.js \
      $src/app.html \
      $src/app.js \
      $src/manifest.webmanifest \
      $src/main.css \
      $src/images/* \
      $src/fonts/* \
      $src/js-aruco/* \
      $out

    cp -R ${fontawesome}/share/* $out
  '';
}
