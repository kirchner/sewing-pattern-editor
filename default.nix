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

  frontend = callPackage ./frontend {
    inherit elmPackages nodePackages;
    mkDerivation = stdenv.mkDerivation;
  };

  backend = callPackage ./backend {
    inherit haskellPackages;
  };

  assets = callPackage ./assets {
    inherit (stdenv) mkDerivation;
    inherit fetchzip;
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

{

  dockerImage = dockerTools.buildImage {
    name = "sewing-pattern-editor";

    contents = [
      backend
      cacert
    ];

    runAsRoot = ''
      #!${runtimeShell}

      mkdir -p /data/log /data/_build

      cp -R \
        ${frontend}/elm.js \
        ${assets}/* \
        /data/_build

      cp ${startup} /data/startup.sh
    '';

    config = {
      WorkingDir = "/data";
      Entrypoint = [ "${runtimeShell}" "-c" ];
      Cmd = [ "/data/startup.sh" ];
      Env = [
        "CLIENT_ID"
        "CLIENT_SECRET"
        "PORT"
      ];
    };
  };

}
