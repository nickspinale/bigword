{ mkDerivation, base, stdenv, mod-n }:
mkDerivation {
  pname = "bigword";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base mod-n ];
  license = stdenv.lib.licenses.mit;
}
