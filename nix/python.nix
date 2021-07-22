{ pkgs }:

let
  poetryPkgs = pkgs.poetry2nix.mkPoetryPackages {
    projectDir = ../.;
    overrides = pkgs.poetry2nix.overrides.withDefaults (self: super: { });
  };

in

{
  buildInputs = [
    (poetryPkgs.python.withPackages (_: poetryPkgs.poetryPackages))
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.libsodium}/lib
  '';
}
