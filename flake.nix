{
  description = "A basic flake for static site generation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        sitepkgs = import ./. { inherit pkgs; };
      in rec {
        devShells = flake-utils.lib.flattenTree {
          devenv = import ./shell.nix { inherit pkgs; };
        };

        packages = sitepkgs // { default = sitepkgs.site; };

        apps = rec {
          site-gen = flake-utils.lib.mkApp { drv = sitepkgs.generator; };
          default = site-gen;
        };
      }
    );
}
