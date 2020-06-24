
let
  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "cc6cf0a96a627e678ffc996a8f9d1416200d6c81";
    };


  nixpkgs =
    import pinnedNix {};

  inherit (nixpkgs.stdenv.lib) optionals;
  inherit (nixpkgs)stdenv;
in

with nixpkgs;

mkShell {
  buildInputs = with pkgs; [
    nodejs
   ];
}
