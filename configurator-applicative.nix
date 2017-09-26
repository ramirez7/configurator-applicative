let
  pkgs = import nix/pkgs.nix;
  overrides = import nix/overrides.nix;
in (pkgs.haskell.packages.ghc802.override {
   overrides = self: super: {
     configurator = self.callCabal2nix "configurator" overrides.configurator {};
     s-cargot = self.callCabal2nix "s-cargot" overrides.s-cargot {};

   };
}).callCabal2nix "configurator-applicative" ./. {}
