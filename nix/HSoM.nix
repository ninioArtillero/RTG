{ mkDerivation, array, arrows, base, containers, deepseq, Euterpea
, fetchgit, ghc-prim, HCodecs, lib, markov-chain, pure-fft, random
, UISF
}:
mkDerivation {
  pname = "HSoM";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/ninioArtillero/HSoM";
    sha256 = "1k9m0fgc4s3anqqpzf47asp7q177zwl04w845yjixixaivq4vmhy";
    rev = "bec36b35fde361d3ce85ba384520a351a247198b";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array arrows base containers deepseq Euterpea ghc-prim HCodecs
    markov-chain pure-fft random UISF
  ];
  homepage = "http://haskell.cs.yale.edu/";
  description = "Library for computer music education";
  license = lib.licenses.bsd3;
}
