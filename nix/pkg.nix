{ mkDerivation, array, attoparsec, base, bytestring, containers
, directory, hspec, hspec-discover, lib, MemoTrie, pretty-simple
, protolude, QuickCheck, text, unordered-containers, vector
}:
mkDerivation {
  pname = "aoc2024";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array attoparsec base bytestring containers directory MemoTrie
    pretty-simple protolude text unordered-containers vector
  ];
  executableHaskellDepends = [
    array attoparsec base bytestring containers directory MemoTrie
    pretty-simple protolude text unordered-containers vector
  ];
  testHaskellDepends = [
    array attoparsec base bytestring containers directory hspec
    MemoTrie pretty-simple protolude QuickCheck text
    unordered-containers vector
  ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.asl20;
  mainProgram = "aoc2024";
}
