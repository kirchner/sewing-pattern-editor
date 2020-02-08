with (
  import (
    builtins.fetchTarball {
      name = "nixos-unstable-2020-02-08";
      url = https://github.com/nixos/nixpkgs/archive/338c988c9a1447ea045ee123ba204da0e26ea1c8.tar.gz;
      sha256 = "052z61v96q199ra4z5zi896m0a2bfjz59gidad0z5z8m9hvjzf1p";
    }
  ) {}
);

let

  frontend = stdenv.mkDerivation {
    name = "frontend";
    src = ./.;

    buildInputs = [
      elmPackages.elm
      nodePackages.uglify-js
    ];

    buildPhase = elmPackages.fetchElmDeps {
      elmPackages = import ./elm-srcs.nix;
      registryDat = ./registry.dat;
      elmVersion = "0.19.1";
    };

    installPhase = ''
      elm make src/frontend/Main.elm --output=elm.js --optimize

      uglifyjs elm.js \
        --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
        --output=elm.min.js

      mkdir $out

      uglifyjs elm.min.js \
        --mangle \
        --output=$out/elm.js
    '';
  };

  backend = haskellPackages.callPackage ./run-server.nix {};

  fontawesome = stdenv.mkDerivation rec {
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

  content = stdenv.mkDerivation {
    name = "sewing-pattern-editor";
    src = ./.;

    dontBuild = true;

    installPhase = ''
      mkdir -p $out/_build

      cp \
        ${frontend}/elm.js \
        $src/assets/service-worker.js \
        $src/assets/register-service-worker.js \
        $src/assets/app.html \
        $src/assets/app.js \
        $src/assets/manifest.webmanifest \
        $src/assets/main.css \
        $src/assets/images/* \
        $src/assets/fonts/* \
        $src/assets/js-aruco/* \
        $out/_build

      cp -R ${fontawesome}/share/* $out/_build


      mkdir -p $out/bin

      cp ${backend}/bin/run-server $out/bin
      cp ${startup} $out/bin/startup.sh
    '';
  };

  startup = writeScript "startup.sh" ''
    #!${runtimeShell}

    _term() {
      echo "Caught SIGTERM signal!"
      kill -TERM "$child" 2>/dev/null
    }

    trap _term SIGTERM
    trap _term SIGKILL
    trap _term SIGINT

    /bin/run-server \
      --environment=production \
      --port=$PORT \
      --clientid=$CLIENT_ID \
      --clientsecret=$CLIENT_SECRET &

    child=$!
    wait "$child"
  '';

in

dockerTools.buildImage {
  name = "sewing-pattern-editor";

  contents = [
    content
    cacert
  ];

  runAsRoot = ''
    #!${runtimeShell}
    mkdir -p /data/log
    cp -R ${content}/_build /data
  '';

  config = {
    WorkingDir = "/data";
    Entrypoint = [ "${runtimeShell}" "-c" ];
    Cmd = [ "/bin/startup.sh" ];
    Env = [
      "CLIENT_ID"
      "CLIENT_SECRET"
      "PORT"
    ];
  };
}
