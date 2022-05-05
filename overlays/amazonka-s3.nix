{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, case-insensitive, fetchgit, lens, lib, tasty, tasty-hunit, text
, time, unordered-containers
}:
mkDerivation {
  pname = "amazonka-s3";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "0fh43r6qfyl1r5y25kic3vv5cxbv9kg15zin2hs67qlg61rc40l0";
    rev = "f1971eb2ef8358c06699cdc105da5d32a251d270";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/services/amazonka-s3; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ amazonka-core base lens text ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring case-insensitive tasty
    tasty-hunit text time unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon Simple Storage Service SDK";
  license = lib.licenses.mpl20;
}
