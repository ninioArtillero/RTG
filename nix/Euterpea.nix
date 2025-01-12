{ mkDerivation, array, arrows, base, bytestring, containers
, deepseq, fetchgit, ghc-prim, HCodecs, heap, lib, PortMidi, random
, stm
}:
mkDerivation {
  pname = "Euterpea";
  version = "2.0.8";
  src = fetchgit {
    url = "https://github.com/ninioArtillero/Euterpea2";
    sha256 = "1hh3flasfsqylc0pxg3dljwcajqmzkgzp45cwn0v4cr4m72c40lh";
    rev = "3bbb079bf95f9c36da0bd8850313ae4a8fbee0cf";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array arrows base bytestring containers deepseq ghc-prim HCodecs
    heap PortMidi random stm
  ];
  homepage = "http://www.euterpea.com";
  description = "Library for computer music research and education";
  license = lib.licenses.bsd3;
}
