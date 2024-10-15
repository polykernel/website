{
  lib,
  stdenv,
  site-builder,
  js-beautify,
}:

stdenv.mkDerivation {
  name = "website";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./css
      ./images
      ./pages
      ./posts
      ./templates
      ./CNAME
      ./404.html
      ./index.html
    ];
  };

  phases = [
    "unpackPhase"
    "buildPhase"
    "installPhase"
  ];

  nativeBuildInputs = [
    site-builder
    js-beautify
  ];

  buildPhase = ''
    runHook preBuild
    ${lib.getExe site-builder} build
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir $out
    cp -r _site/* $out
    runHook postInstall
  '';
}
