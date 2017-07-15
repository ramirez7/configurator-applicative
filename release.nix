let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {

          configurator-descriptive =
            haskellPackagesNew.callPackage ./default.nix { };

          s-cargot =
            haskellPackagesNew.callPackage ./scargot.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { configurator-descriptive = pkgs.haskellPackages.configurator-descriptive;
  }
