{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, case-insensitive, fetchgit, lib, tasty, tasty-hunit, text, time
, unordered-containers
}:
mkDerivation {
  pname = "amazonka-sts";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "05rxii4kkhp56xbgbylyj37c58awrf8v10qx8k1cp3ml9blp67ds";
    rev = "d3387129882737add0a30fe2119b71da1afaee4e";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/services/amazonka-sts; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ amazonka-core base ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring case-insensitive tasty
    tasty-hunit text time unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon Security Token Service SDK";
  license = lib.licenses.mpl20;
}
