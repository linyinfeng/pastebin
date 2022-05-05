{ mkDerivation, amazonka, amazonka-core, amazonka-s3, base, binary
, bytestring, classy-prelude, conduit, containers, exceptions
, http-types, lens, lib, MonadRandom, mtl, optparse-applicative
, random, resourcet, scotty, text, wai
}:
mkDerivation {
  pname = "pastebin";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    amazonka amazonka-core amazonka-s3 base binary bytestring
    classy-prelude conduit containers exceptions http-types lens
    MonadRandom mtl random resourcet scotty text wai
  ];
  executableHaskellDepends = [
    amazonka amazonka-core amazonka-s3 base binary bytestring
    classy-prelude conduit containers exceptions http-types lens
    MonadRandom mtl optparse-applicative random resourcet scotty text
    wai
  ];
  description = "A simple pastebin server";
  license = lib.licenses.mit;
}
