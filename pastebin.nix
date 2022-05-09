{ mkDerivation, amazonka, amazonka-s3, base, binary, bytestring
, conduit, containers, exceptions, http-types, lens, lib
, MonadRandom, mtl, neat-interpolation, optparse-applicative
, resourcet, text, wai, wai-extra, warp
}:
mkDerivation {
  pname = "pastebin";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    amazonka amazonka-s3 base binary bytestring conduit containers
    exceptions http-types lens MonadRandom mtl neat-interpolation
    resourcet text wai wai-extra warp
  ];
  executableHaskellDepends = [
    amazonka amazonka-s3 base binary bytestring conduit containers
    exceptions http-types lens MonadRandom mtl neat-interpolation
    optparse-applicative resourcet text wai wai-extra warp
  ];
  description = "A simple pastebin server";
  license = lib.licenses.mit;
}
