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
    sha256 = "0fh43r6qfyl1r5y25kic3vv5cxbv9kg15zin2hs67qlg61rc40l0";
    rev = "f1971eb2ef8358c06699cdc105da5d32a251d270";
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
