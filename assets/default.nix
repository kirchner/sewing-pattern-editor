{ mkDerivation
, fetchzip
, fetchurl
, serviceWorker ? true
}:

let

  fontawesome = mkDerivation rec {
    name = "fontawesome";
    version = "5.12.1";
    src = fetchzip {
      stripRoot = true;
      url = "https://use.fontawesome.com/releases/v${version}/fontawesome-free-${version}-web.zip";
      sha256 = "160s17izj7y0bajaz5d462zi8p1g0b2rrjnmdidsd9rkwja52xm6";
    };

    installPhase =
        ''
          mkdir -p $out/share
          cp -R $src/* $out/share
        '';
  };

  svg2pdf = fetchurl {
    url = https://raw.githubusercontent.com/yWorks/svg2pdf.js/master/dist/svg2pdf.min.js;
    sha256 = "12mngipqm6x9sn0bv2az7sr6s2a3nrn70hsbabdhk51ndmiqj4r9";
  };

  jspdf = fetchurl {
    url = https://raw.githubusercontent.com/yWorks/jsPDF/master/dist/jspdf.min.js;
    sha256 = "1nzf0xgc48n2dr4acclpbmwgsm9jy3bjkwjk3rh4npywk4f129zy";
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
      $src/stories.html \
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
    cp ${jspdf} $out/jspdf.min.js
    cp ${svg2pdf} $out/svg2pdf.min.js
  '';
}
