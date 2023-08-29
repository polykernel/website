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

  src = lib.cleanSource ./.;

  isLibrary = false;

  isExecutable = true;

  executableHaskellDepends = [ base hakyll pandoc ];
  
  license = lib.licenses.mit;

  mainProgram = "hakyll-site";
}