{
  sources ? import ./nix/sources.nix,
  pkgs ? import sources.nixpkgs { },
}:

let
  inherit (pkgs) stdenv lib;

  ghc-version = "ghc96";

  haskellPackages = pkgs.haskell.packages.${ghc-version};

  generator = pkgs.haskellPackages.callPackage (import ./generator) {};

  site = stdenv.mkDerivation {
    name = "site";
    version = "0.1.0";
    
    src = pkgs.nix-gitignore.gitignoreSource [] ./.;
    
    phases = [ "unpackPhase" "buildPhase" "installPhase" ];

    buildInputs = [ generator ];

    # Fixes from https://github.com/rpearce/hakyll-nix-template/blob/main/flake.nix#L63-L66
    # courtesy of Robert Pearce
    LANG = "en_US.UTF-8";
    LOCALE_ARCHIVE = lib.optionalString (stdenv.buildPlatform.libc == "glibc") "${pkgs.glibcLocales}/lib/locale/locale-archive";

    buildPhase = ''
      runHook preBuild
      ${lib.getExe generator} build
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir $out
      cp -r _site/* $out
      runHook postInstall
    '';
  };
in
{ inherit generator site; }
