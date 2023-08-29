{
  description = "A basic flake for static site generation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/main";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        site-pkgs = import ./. { inherit pkgs; };
      in {
        devShells = flake-utils.lib.flattenTree {
          devenv = import ./shell.nix { inherit pkgs; };
        };

        packages = {
          inherit (site-pkgs) site generator;
          default = site-pkgs.site;
        };

        apps = rec {
          generator = flake-utils.lib.mkApp { drv = site-pkgs.generator; };
          default = generator;
        };
      }
    );
}
