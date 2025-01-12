{ mkDerivation, arithmoi, base, clock, containers, Euterpea, groups
, hosc, lib, midair, multiset-comb, QuickCheck, Yampa
}:
mkDerivation {
  pname = "rtg";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    arithmoi base clock containers Euterpea groups hosc midair
    multiset-comb Yampa
  ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://www.github.com/ninioartillero/RTG/";
  description = "A geometric language for rhythmic patterns";
  license = lib.licenses.gpl3Plus;
}
