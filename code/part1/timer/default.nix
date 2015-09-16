{ mkDerivation, base, lens, linear, mtl, sdl2, stdenv, stm }:
mkDerivation {
  pname = "refactor-timer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base lens linear mtl sdl2 stm ];
  executableHaskellDepends = [ base sdl2 ];
  homepage = "http://dlaing.org/falling-blocks";
  license = stdenv.lib.licenses.bsd2;
}
