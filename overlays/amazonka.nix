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
    sha256 = "05rxii4kkhp56xbgbylyj37c58awrf8v10qx8k1cp3ml9blp67ds";
    rev = "d3387129882737add0a30fe2119b71da1afaee4e";
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
