{
  description = "A basic flake for generating a static site";

  # nixConfig = {
  #   bash-prompt = "\\n\\[\\033[1;33m\\][\\[\\e]0;nix-develop: \\w\\a\\]nix-develop:\\w]λ \\[\\033[0m\\]";
  # };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShells = flake-utils.lib.flattenTree {
          devenv = import ./shell.nix { inherit pkgs; };
        };
      }
    );
}
