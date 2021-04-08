let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { system = "x86_64-linux"; };
  ligoBinary =
    # Run 'niv update ligo-artifacts -r <git_rev>' to update
    pkgs.runCommand "ligo-binary" { buildInputs = [ pkgs.unzip ]; } ''
      mkdir -p $out/bin
      unzip ${sources.ligo-artifacts} ligo -d $out/bin
      chmod +x $out/bin/ligo
    '';
in
{
  michelson = pkgs.stdenv.mkDerivation {
    name = "huxian-michelson";
    buildInputs = [ ligoBinary ];
    src = pkgs.lib.cleanSource ./.;
    buildPhase = ''
      make build-ligo
    '';
    installPhase = ''
      mkdir -p $out
      cp generated/michelson/*.tz $out
    '';
  };
}
