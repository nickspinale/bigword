{ mkDerivation, base, binary, bytestring, HUnit, mod-n, QuickCheck
, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, data-type-util
}:
mkDerivation {
  pname = "bigword";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base mod-n data-type-util ];
  testHaskellDepends = [
    base binary bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  license = stdenv.lib.licenses.mit;
}
