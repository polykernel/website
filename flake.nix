{
  description = "Basic flake for static site generation";

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://attic.polykernel.cc/website"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "website:RLkaxRV2fh6geBXFKGl3DQorHdxhGM4KiXbcpOQvLSU="
    ];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      haskellNix,
      flake-utils,
      pre-commit-hooks,
      ...
    }@inputs:
    let
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
                compiler-nix-name = "ghc96";
                modules = [ { doHaddock = false; } ];
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
          apps = {
            site-builder = flake-utils.mkApp { drv = site-builder; };
            default = _self.apps.site-builder;
          };

          checks = {
            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                treefmt.enable = true;
                treefmt.settings = {
                  formatters = [
                    pkgs.nixfmt-rfc-style
                    pkgs.typos
                    pkgs.toml-sort
                  ];
                };
              };
            };
          };

          devShells = {
            builder-shell = project.shellFor {
              inherit (_self.checks.pre-commit-check) shellHook;
              tools = {
                cabal = "latest";
                cabal-fmt = "latest";
                haskell-language-server = "latest";
                hlint = "latest";
                ormolu = "latest";
              };

              nativeBuildInputs = [
                pkgs.nodePackages.js-beautify
              ] ++ _self.checks.pre-commit-check.enabledPackages;

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
