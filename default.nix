{ mkDerivation, base, binary, bytestring, HUnit, mod-n, QuickCheck
, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "bigword";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base mod-n ];
  testHaskellDepends = [
    base binary bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  license = stdenv.lib.licenses.mit;
}
