{
  pkgs ? import <nixpkgs> {}
}:

let
  inherit (pkgs) stdenv lib;

  compilerVersion = "ghc96";

  compilerPackages = pkgs.haskell.packages.${compilerVersion};
in
(compilerPackages.developPackage {
  name = "website";
  root = lib.fileset.toSource (
    let
      fsroot = ./generator;
      mkFiles = map (f: fsroot + f);
    in {
      root = fsroot;
      fileset = lib.fileset.unions (mkFiles [
        "/LICENSE"
        "/site.hs"
        "/website.cabal"
      ]);
    }
  );

  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv [
      compilerPackages.cabal-install
      compilerPackages.hakyll
      compilerPackages.pandoc
      pkgs.nodePackages.js-beautify
    ];
}).overrideAttrs (final: prev: {
  # Fixes from https://github.com/rpearce/hakyll-nix-template/blob/main/flake.nix#L63-L66
  # courtesy of Robert Pearce
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = lib.optionalString (stdenv.buildPlatform.libc == "glibc") "${pkgs.glibcLocales}/lib/locale/locale-archive";
})
