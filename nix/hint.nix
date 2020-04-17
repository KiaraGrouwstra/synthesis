{ mkDerivation, base, containers, directory, exceptions
, extensible-exceptions, fetchgit, filepath, ghc, ghc-boot
, ghc-paths, HUnit, random, stdenv, temporary, transformers, unix
}:
mkDerivation {
  pname = "hint";
  version = "0.9.0.2";
  src = fetchgit {
    url = "https://github.com/haskell-hint/hint.git";
    sha256 = "0dxv0g21klhaa441kr40zwidpnbh94p97m1sqvk52agn93s6gsjr";
    rev = "dbb878955528d9125a601773b25a3c45b9fcef06";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base directory exceptions filepath ghc ghc-boot ghc-paths random
    temporary transformers unix
  ];
  testHaskellDepends = [
    base containers directory exceptions extensible-exceptions filepath
    HUnit unix
  ];
  homepage = "https://github.com/haskell-hint/hint";
  description = "Runtime Haskell interpreter (GHC API wrapper)";
  license = stdenv.lib.licenses.bsd3;
}
