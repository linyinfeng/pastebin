#!/usr/bin/env bash

cabal2nix . > pastebin.nix

pushd overlays

function cabal2nix-amazonka {
    cabal2nix https://github.com/brendanhay/amazonka.git --subpath "$1" > "$2"
}

cabal2nix-amazonka lib/amazonka              amazonka.nix
cabal2nix-amazonka lib/amazonka-core         amazonka-core.nix
cabal2nix-amazonka lib/amazonka-test         amazonka-test.nix
cabal2nix-amazonka lib/services/amazonka-s3  amazonka-s3.nix
cabal2nix-amazonka lib/services/amazonka-sts amazonka-sts.nix
cabal2nix-amazonka lib/services/amazonka-sso amazonka-sso.nix

popd

nix flake update
