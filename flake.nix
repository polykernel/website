{
  description = "Basic flake for static site generation";

  nixConfig = {
    # allow-import-from-derivation = "true";
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      haskellNix,
      flake-utils,
      ...
    }@inputs:
    let
      compilerVersion = "ghc96";

      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      perSystem =
        system:
        let
          overlays = [
            haskellNix.overlay
            (final: prev: {
              hakyllProject = final.haskell-nix.project' {
                src = ./builder;
                compiler-nix-name = "ghc948";
                modules = [ { doHaddock = false; } ];
                shell.buildInputs = [
                  site-builder
                ];
                shell.tools = {
                  cabal = "latest";
                  hlint = "latest";
                  haskell-language-server = "latest";
                };
              };
            })
          ];

          pkgs = import nixpkgs {
            inherit overlays system;
            inherit (haskellNix) config;
          };

          project = pkgs.hakyllProject;

          flake = project.flake { };

          executable = "builder:exe:site-builder";

          site-builder = flake.packages.${executable};

          website = pkgs.callPackage ./website.nix {
            inherit (pkgs.nodePackages) js-beautify;
            inherit site-builder;
          };
        in
        flake
        // nixpkgs.lib.fix (_self: {
          devShells = {
            builder-shell = project.shellFor {
              tools = {
                cabal = "latest";
                hlint = "latest";
                haskell-language-server = "latest";
              };

              nativeBuildInputs = with pkgs; [
                nodePackages.js-beautify
              ];

              exactDeps = true;
            };
            default = _self.devShells.builder-shell;
          };
          packages = {
            inherit site-builder website;
            default = _self.packages.website;
          };
        });
    in
    flake-utils.lib.eachSystem supportedSystems perSystem;
}
