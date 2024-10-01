{ mkDerivation, arithmoi, base, containers, groups, hosc, lib
, multiset-comb, QuickCheck
}:
mkDerivation {
  pname = "rtg";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    arithmoi base containers groups hosc multiset-comb
  ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://www.github.com/ninioartillero/RTG/";
  description = "A geometric language for rhythmic patterns";
  license = lib.licenses.gpl3Only;
}
