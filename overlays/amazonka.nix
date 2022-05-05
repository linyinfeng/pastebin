{ mkDerivation, aeson, amazonka-core, amazonka-sso, amazonka-sts
, base, bytestring, conduit, directory, exceptions, fetchgit
, http-client, http-conduit, http-types, ini, lens, lib, resourcet
, retry, text, time, transformers, unordered-containers, uuid
}:
mkDerivation {
  pname = "amazonka";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka.git";
    sha256 = "0fh43r6qfyl1r5y25kic3vv5cxbv9kg15zin2hs67qlg61rc40l0";
    rev = "f1971eb2ef8358c06699cdc105da5d32a251d270";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/amazonka; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson amazonka-core amazonka-sso amazonka-sts base bytestring
    conduit directory exceptions http-client http-conduit http-types
    ini lens resourcet retry text time transformers
    unordered-containers uuid
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Comprehensive Amazon Web Services SDK";
  license = lib.licenses.mpl20;
}
