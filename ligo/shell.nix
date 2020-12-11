let
  sources = import ../nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
  # This revision contain a fix to https://gitlab.com/ligolang/ligo/-/issues/1066#note_458556689
  ligoPkgs =
    let ligoSrc = pkgs.fetchgit {
      url = "https://gitlab.com/ligolang/ligo";
      rev = "6d68d6258566e398a321424f536d037be536cf96";
      sha256 = "sha256-idlJ8yNSpAlDXgctsp0/no2/ozgLKoMwQrLae1WHp44="; };
    in (import "${ligoSrc}/nix/pkgs.nix" {});
in
pkgs.mkShell {
  name = "huxian-ligo";
  buildInputs = with pkgs.ocamlPackages; [
    ligoPkgs.ligo-bin
  ];
}
