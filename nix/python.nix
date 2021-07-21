{ pkgs }:

let
  poetryPkgs = pkgs.poetry2nix.mkPoetryPackages {
    projectDir = ../.;
    overrides = pkgs.poetry2nix.overrides.withDefaults (self: super: {
      msgpack = super.msgpack.overridePythonAttrs (old: {
        doCheck = false;
      });
      # upstream issue: https://github.com/nix-community/poetry2nix/issues/306
      pendulum = super.pendulum.override {
        preferWheel = true;
      };
      pytezos = super.pytezos.override (old: {
        buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.libsodium ];
      });
      matplotlib = super.matplotlib.overridePythonAttrs (
      old: {
        propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ [ self.certifi ];
      });
      # TODO: Upstream these overrides where possible.
    });
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
