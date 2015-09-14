with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        sdl2 = self.callPackage ../../../../sdl2 {};
        falling-blocks-cycle = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.falling-blocks-cycle.env
