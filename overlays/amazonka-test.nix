{ mkDerivation, aeson, amazonka-core, base, bifunctors, bytestring
, case-insensitive, conduit, conduit-extra, fetchgit, groom
, http-client, http-types, lib, process, resourcet, tasty
, tasty-hunit, template-haskell, temporary, text, time
, unordered-containers, yaml
}:
mkDerivation {
  pname = "amazonka-test";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "05rxii4kkhp56xbgbylyj37c58awrf8v10qx8k1cp3ml9blp67ds";
    rev = "d3387129882737add0a30fe2119b71da1afaee4e";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/amazonka-test; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson amazonka-core base bifunctors bytestring case-insensitive
    conduit conduit-extra groom http-client http-types process
    resourcet tasty tasty-hunit template-haskell temporary text time
    unordered-containers yaml
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Common functionality for Amazonka library test-suites";
  license = lib.licenses.mpl20;
}
