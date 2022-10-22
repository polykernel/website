{ pkgs ? import <nixpkgs> { } }:

let
  inherit (pkgs) stdenv lib;
in rec {
  generator = stdenv.mkDerivation {
    name = "hakyll-site";

    src = ./src;

    phases = [ "unpackPhase" "buildPhase" ];

    buildInputs = [ (pkgs.haskellPackages.ghcWithPackages (p: with p; [ hakyll pandoc ])) ];

    buildPhase = ''
      runHook preBuild

      mkdir -p $out/bin
      ghc -O2 -dynamic --make Main.hs -o $out/bin/hakyll-site

      runHook postBuild
    '';
  };

  site = stdenv.mkDerivation {
    name = "site";
    version = "0.1";
    
    src = lib.cleanSource ./.;
    
    phases = [ "unpackPhase" "buildPhase" ];

    buildInputs = [ generator ];

    buildPhase = ''
      runHook preBuild

      export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LANG=en_US.UTF-8

      hakyll-site build

      mkdir $out
      cp -r _site/* $out

      runHook postBuild
    '';
  };
}
