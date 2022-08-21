{ mkDerivation, aeson, attoparsec, base, bytestring
, case-insensitive, conduit, conduit-extra, containers, cryptonite
, data-ordlist, deepseq, fetchgit, hashable, http-client
, http-conduit, http-types, lens, lib, memory, mtl, QuickCheck
, quickcheck-unicode, regex-posix, resourcet, scientific, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, text, time
, transformers, unordered-containers, xml-conduit, xml-types
}:
mkDerivation {
  pname = "amazonka-core";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "05rxii4kkhp56xbgbylyj37c58awrf8v10qx8k1cp3ml9blp67ds";
    rev = "d3387129882737add0a30fe2119b71da1afaee4e";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/amazonka-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base bytestring case-insensitive conduit
    conduit-extra containers cryptonite deepseq hashable http-client
    http-conduit http-types lens memory mtl regex-posix resourcet
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
