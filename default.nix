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

  elmJs = debug: callPackage ./frontend {
    inherit (stdenv) mkDerivation;
    inherit elmPackages nodePackages debug;
  };

  backend = callPackage ./backend {
    inherit haskellPackages;
  };

  assets = serviceWorker: callPackage ./assets {
    inherit (stdenv) mkDerivation;
    inherit fetchzip serviceWorker;
  };

in

{

  debug = mkShell {
    shellHook = ''
      function server {
        ${backend}/bin/run-server \
          --assets=${assets false} \
          --frontend=${elmJs true} \
          --debug \
          --port=4321
      }
    '';
  };


  dockerImage = dockerTools.buildImage {
    name = "sewing-pattern-editor";

    contents = cacert;

    runAsRoot = ''
      #!${runtimeShell}
      mkdir -p /data/log
    '';

    config = {
      Env = [
        "CLIENT_ID"
        "CLIENT_SECRET"
        "PORT"
      ];
      WorkingDir = "/data";
      Entrypoint = [ "${runtimeShell}" "-c" ];

      Cmd = [
        (
          writeScript "startup.sh" ''
            #!${runtimeShell}

            _term() {
              echo "Caught SIGTERM signal!"
              kill -TERM "$child" 2>/dev/null
            }

            trap _term SIGTERM
            trap _term SIGKILL
            trap _term SIGINT

            ${backend}/bin/run-server \
              --frontend=${elmJs false} \
              --assets=${assets true} \
              --port=$PORT \
              --clientid=$CLIENT_ID \
              --clientsecret=$CLIENT_SECRET &

            child=$!
            wait "$child"
          ''
          )
      ];
    };
  };

}
