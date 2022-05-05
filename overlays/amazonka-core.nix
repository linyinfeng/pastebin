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
    sha256 = "0fh43r6qfyl1r5y25kic3vv5cxbv9kg15zin2hs67qlg61rc40l0";
    rev = "f1971eb2ef8358c06699cdc105da5d32a251d270";
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
