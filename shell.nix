{ pkgs ? import <nixpkgs> { },
  pastebin ? import ./default.nix { inherit pkgs; }
}:

with pkgs;

mkShell {
  inputsFrom = [
    pastebin
  ];
  packages = [
    cabal-install
    ghc
    zlib
    haskell-language-server
    ormolu
    haskellPackages.cabal-fmt
    haskellPackages.prune-juice
    fd
  ];
}
