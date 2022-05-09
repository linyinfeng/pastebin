{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, case-insensitive, fetchgit, lens, lib, tasty, tasty-hunit, text
, time, unordered-containers
}:
mkDerivation {
  pname = "amazonka-s3";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "1rg83628v6k41ckrz1ivnnkcdq81j6pk0l8nhkwf77ipr9l0i5jk";
    rev = "27322ea0db68712e1c331be4124080144c975750";
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
