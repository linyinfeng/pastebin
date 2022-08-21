{ mkDerivation, amazonka, amazonka-core, amazonka-s3, base, binary
, bytestring, conduit, containers, either, exceptions, http-types
, lens, lib, magic, MonadRandom, mtl, neat-interpolation
, optparse-applicative, resourcet, text, wai, wai-extra, warp
}:
mkDerivation {
  pname = "pastebin";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    amazonka amazonka-core amazonka-s3 base binary bytestring conduit
    containers either exceptions http-types lens magic MonadRandom mtl
    neat-interpolation resourcet text wai wai-extra warp
  ];
  executableHaskellDepends = [
    amazonka amazonka-core amazonka-s3 base binary bytestring conduit
    containers either exceptions http-types lens magic MonadRandom mtl
    neat-interpolation optparse-applicative resourcet text wai
    wai-extra warp
  ];
  description = "A simple pastebin server";
  license = lib.licenses.mit;
  mainProgram = "pastebin";
}
