{
  mkDerivation,
  lib,
  base,
  hakyll,
  pandoc,
}:

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

  executableHaskellDepends = [ base hakyll pandoc ];
  
  license = lib.licenses.mit;

  mainProgram = "hakyll-site";
}