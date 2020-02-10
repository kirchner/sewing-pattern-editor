{ mkDerivation
, elmPackages
, nodePackages
}:

mkDerivation {
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
    elm make src/Main.elm --output=elm.js --optimize

    uglifyjs elm.js \
      --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
      --output=elm.min.js

    mkdir $out

    uglifyjs elm.min.js \
      --mangle \
      --output=$out/elm.js
  '';
}
