{
  pkgs ? import <nixpkgs> { },
  pastebin ? import ./default.nix { inherit pkgs; },
}:

with pkgs;

mkShell {
  inputsFrom = [ pastebin ];
  packages = [
    cabal-install
    ghc
    zlib
    file # libmagic
    haskell-language-server
    ormolu
    haskellPackages.cabal-fmt
    fd
  ];
}
