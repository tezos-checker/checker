let sources = import ../nix/sources.nix {};
in

{ pkgs ? import sources.nixpkgs {} }:

let
poetryPkgs = pkgs.poetry2nix.mkPoetryPackages {
  projectDir = ./.;
  overrides = poetry2nix.overrides.withDefaults (self: super: {
    # remove eth-hash dependency because eth-hash also depends on eth-utils causing a cycle.
    eth-utils = super.eth-utils.overridePythonAttrs (old: {
      propagatedBuildInputs =
        builtins.filter (i: i.pname != "eth-hash") old.propagatedBuildInputs;
      preConfigure = ''
        ${old.preConfigure or ""}
        sed -i '/eth-hash/d' setup.py
      '';
    });
    msgpack = super.msgpack.overridePythonAttrs (old: {
      doCheck = false;
    });
    # our setuptools version is for some reason x.y.post0 which confuses a check
    # inside secp256k1.
    secp256k1 = super.secp256k1.overridePythonAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.pkgconfig pkgs.autoconf pkgs.automake pkgs.libtool ];
      buildInputs = (old.buildInputs or [ ]) ++ [ self.pytest-runner ];
      doCheck = false;
      postPatch = ''
        substituteInPlace setup.py \
          --replace 'setuptools_version.' '"${self.setuptools.version}".'
      '';
    });
    fastecdsa = super.fastecdsa.overridePythonAttrs (old: {
      buildInputs = old.buildInputs ++ [ pkgs.gmp.dev ];
    });
    pendulum = super.pendulum.override {
      preferWheel = true;
    };
    pytezos = super.pytezos.override (old: {
      buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.libsodium ];
    });
  });
};

in
pkgs.mkShell {
  buildInputs = [
    (poetryPkgs.python.withPackages (_: poetryPkgs.poetryPackages))
    pkgs.libsodium.dev
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.libsodium}/lib
  '';
}
