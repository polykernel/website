{
  lib,
  stdenv,
  site-builder,
  js-beautify,
  glibcLocalesUtf8,
  librsvg,
  imagemagick,
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
      ./icon.svg
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
    librsvg
    imagemagick
  ];

  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = lib.optionalString (
    stdenv.buildPlatform.libc == "glibc"
  ) "${lib.getLib glibcLocalesUtf8}/lib/locale/locale-archive";

  buildPhase = ''
    runHook preBuild

    # Build the site
    ${lib.getExe site-builder} build

    # Format html files
    echo 'Post-processing'
    find _site -type f -name '*.html' -exec '${lib.getExe' js-beautify "js-beautify"}' -f '{}' -r --type html --no-preserve-newlines \;

    # Create favicons
    rsvg-convert -h 16 -o icon-16.png icon.svg
    rsvg-convert -h 32 -o icon-32.png icon.svg
    magick icon-16.png icon-32.png _site/favicon.ico
    rsvg-convert -h 192 -o icon-192.png icon.svg
    magick icon-192.png -resize 140x140 -gravity center -background transparent -extent 180x180 -quality 100 _site/apple-touch-icon.png

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir $out
    cp -r _site/* $out

    runHook postInstall
  '';
}
