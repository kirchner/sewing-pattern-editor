{ mkDerivation
, fetchzip
, serviceWorker ? true
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

    installPhase =
        ''
          mkdir -p $out/share
          cp -R $src/fontawesome-free-${version}-web/* $out/share
        '';
  };

in

mkDerivation {
  name =
    if serviceWorker then
      "assets"

    else
      "assets-without-service-worker";

  src = ./.;

  dontBuild = true;

  installPhase = ''
    mkdir $out

    cp \
      $src/app.html \
      $src/app.js \
      $src/manifest.webmanifest \
      $src/main.css \
      $src/images/* \
      $src/fonts/* \
      $src/js-aruco/* \
      $out

    ${if serviceWorker then
        "cp $src/service-worker.js $out"

      else
        ""}

    cp $src/${
      if serviceWorker then
        "register-service-worker.js"

      else
        "register-service-worker--no-op.js"} \
      $out/register-service-worker.js

    cp -R ${fontawesome}/share/* $out
  '';
}
