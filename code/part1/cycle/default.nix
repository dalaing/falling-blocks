{ mkDerivation, base, linear, sdl2, stdenv }:
mkDerivation {
  pname = "refactor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base linear sdl2 ];
  executableHaskellDepends = [ base linear sdl2 ];
  homepage = "http://dlaing.org/falling-blocks";
  license = stdenv.lib.licenses.bsd2;
}
