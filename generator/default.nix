{
  mkDerivation,
  lib,
  base,
  hakyll,
  pandoc,
  makeWrapper,
  js-beautify,
}:

# NB: this is haskellPackages.mkDerivation not stdenv.mkDerivation
mkDerivation {
  pname = "website";

  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./site.hs
      ./website.cabal
      ./LICENSE
    ];
  };

  isLibrary = false;

  isExecutable = true;

  doHaddock = false;

  buildTools = [ makeWrapper ];

  executableHaskellDepends = [ base hakyll pandoc ];

  postInstall = ''
    wrapProgram $out/bin/hakyll-site \
      --prefix PATH : ${lib.makeBinPath [ js-beautify ]}
  '';

  license = lib.licenses.mit;

  mainProgram = "hakyll-site";
}
