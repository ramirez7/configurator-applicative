{ mkDerivation, base, containers, parsec, QuickCheck, stdenv, text
}:
mkDerivation {
  pname = "s-cargot";
  version = "0.1.2.0";
  sha256 = "068ysjnparlrwrppqxi91yxqpskz8nqzyypp3qnad04fyz3l36mm";
  revision = "2";
  editedCabalFile = "331250903dfefd8f198eace1f0039bf79da6191b07edb4bb532032cb44bbf045";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers parsec text ];
  testHaskellDepends = [ base parsec QuickCheck text ];
  homepage = "https://github.com/aisamanra/s-cargot";
  description = "A flexible, extensible s-expression library";
  license = stdenv.lib.licenses.bsd3;
}
