with (import <nixpkgs> {});

{
  run-server = haskellPackages.callPackage ./run-server.nix {};
}
