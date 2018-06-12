{ mkDerivation, base, lens, stdenv, trifecta }:
mkDerivation {
  pname = "bool-expr";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens trifecta ];
  license = stdenv.lib.licenses.bsd3;
}
