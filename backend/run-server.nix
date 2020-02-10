{ mkDerivation, aeson, base, bytestring, cmdargs, http-client
, http-client-tls, mtl, snap-core, snap-server, stdenv
, unordered-containers
}:
mkDerivation {
  pname = "run-server";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring cmdargs http-client http-client-tls mtl
    snap-core snap-server unordered-containers
  ];
  homepage = "https://sewing-pattern-editor.herokuapp.com";
  description = "Server for the sewing pattern editor";
  license = stdenv.lib.licenses.agpl3;
}
