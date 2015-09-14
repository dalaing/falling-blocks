with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        sdl2 = self.callPackage ../../../../sdl2 {};
        falling-blocks-mtl = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.falling-blocks-mtl.env
