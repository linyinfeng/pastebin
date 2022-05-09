{ mkDerivation, aeson, attoparsec, base, bytestring
, case-insensitive, conduit, conduit-extra, containers, cryptonite
, data-ordlist, deepseq, fetchgit, hashable, http-client
, http-conduit, http-types, lens, lib, memory, QuickCheck
, quickcheck-unicode, regex-posix, resourcet, scientific, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, text, time
, transformers, unordered-containers, xml-conduit, xml-types
}:
mkDerivation {
  pname = "amazonka-core";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "1rg83628v6k41ckrz1ivnnkcdq81j6pk0l8nhkwf77ipr9l0i5jk";
    rev = "27322ea0db68712e1c331be4124080144c975750";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/amazonka-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base bytestring case-insensitive conduit
    conduit-extra containers cryptonite deepseq hashable http-client
    http-conduit http-types lens memory regex-posix resourcet
    scientific text time transformers unordered-containers xml-conduit
    xml-types
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive conduit data-ordlist
    http-conduit http-types lens QuickCheck quickcheck-unicode tasty
    tasty-hunit tasty-quickcheck template-haskell text time
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Core data types and functionality for Amazonka libraries";
  license = lib.licenses.mpl20;
}
