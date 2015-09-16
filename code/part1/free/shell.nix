with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        sdl2 = self.callPackage ../../../../sdl2 {};
        falling-blocks-free = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.falling-blocks-free.env
