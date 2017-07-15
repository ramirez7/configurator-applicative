{ mkDerivation, base, configurator, containers, mtl, s-cargot
, stdenv, text
}:
mkDerivation {
  pname = "configurator-descriptive";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base configurator containers mtl s-cargot text
  ];
  license = stdenv.lib.licenses.bsd3;
}
