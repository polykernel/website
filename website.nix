{
  lib,
  stdenv,
  site-builder,
  js-beautify,
  glibcLocalesUtf8,
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

  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = lib.optionalString (stdenv.buildPlatform.libc == "glibc") "${lib.getLib glibcLocalesUtf8}/lib/locale/locale-archive";

  buildPhase = ''
    runHook preBuild
    # Build the site
    ${lib.getExe site-builder} build
    # Format html files
    echo 'Post-processing'
    find _site -type f -name '*.html' -exec '${lib.getExe' js-beautify "js-beautify"}' -f '{}' -r --type html --no-preserve-newlines \;
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir $out
    cp -r _site/* $out
    runHook postInstall
  '';
}
