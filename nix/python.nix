{ pkgs }:

let
poetryPkgs = pkgs.poetry2nix.mkPoetryPackages {
  projectDir = ../.;
  overrides = pkgs.poetry2nix.overrides.withDefaults (self: super: {
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
    # TODO: Upstream these overrides where possible.

    # Below is a workaround until https://github.com/nix-community/poetry2nix/pull/304
    markupsafe = super.markupsafe.override (old: {
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/67/6a/5b3ed5c122e20c33d2562df06faf895a6b91b0a6b96a4626440ffe1d5c8e/MarkupSafe-2.0.0.tar.gz";
        hash = "sha256-T64Gd/cS7gkHIdixf0EvHLzu+/DcGA/pG6syMvOLRSc=";
      };
    });
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
