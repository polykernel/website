{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) stdenv lib;

  compilerVersion = "ghc946";

  compilerPackages = pkgs.haskell.packages.${compilerVersion};
in
(compilerPackages.developPackage {
  name = "website";
  root = ./generator;

  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with compilerPackages;
      [
        cabal-install
        hakyll
        pandoc
      ]
    );
}).overrideAttrs (final: prev: {
  # Fixes from https://github.com/rpearce/hakyll-nix-template/blob/main/flake.nix#L63-L66
  # courtesy of Robert Pearce
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = lib.optionalString (stdenv.buildPlatform.libc == "glibc") "${pkgs.glibcLocales}/lib/locale/locale-archive";
})
