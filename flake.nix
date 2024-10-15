{
  description = "Basic flake for static site generation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs:
    let
      compilerVersion = "ghc98";

      overlay = final: prev: {
        myHaskellPackages = prev.haskell.packages.${compilerVersion}.override {
          overrides = hfinal: hprev: {
            builder = hprev.callCabal2nix "builder" ./builder { };
            pandoc = hprev.pandoc_3_3;
            texmath = hprev.texmath_0_12_8_9;
            tls = hprev.tls_2_1_0;
            typst = hprev.typst_0_5_0_5;
            typst-symbols = hprev.typst-symbols_0_1_6;
            crypton-connection = hprev.crypton-connection_0_4_1;
            toml-parser = hprev.toml-parser_2_0_1_0;
          };
        };
      };

      perSystem =
        system:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };

          hsPkgs = pkgs.myHaskellPackages;
          hsLib = pkgs.haskell.lib;

          builder = hsLib.compose.justStaticExecutables hsPkgs.builder;

          builder-shell = hsPkgs.shellFor {
            withHoogle = false;
            packages = p: [ p.builder ];
            buildInputs = [
              hsPkgs.cabal-install
              hsPkgs.haskell-language-server
              hsPkgs.hlint
              hsPkgs.ormolu
              pkgs.nodePackages.js-beautify
            ];
          };

          website = pkgs.callPackage ./website.nix {
            inherit (pkgs.nodePackages) js-beautify;
            site-builder = builder;
          };
        in
        {
          devShells = {
            inherit builder-shell;
            default = builder-shell;
          };
          packages = {
            inherit builder website;
            default = website;
          };
        };
    in
    inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
