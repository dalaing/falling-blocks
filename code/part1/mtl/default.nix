{ mkDerivation, base, linear, mtl, sdl2, stdenv }:
mkDerivation {
  pname = "refactor-mtl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base linear mtl sdl2 ];
  executableHaskellDepends = [ base mtl sdl2 ];
  homepage = "http://dlaing.org/falling-blocks";
  license = stdenv.lib.licenses.bsd2;
}
