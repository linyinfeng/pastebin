{
  pkgs ? import <nixpkgs> { },
}:

with pkgs;

haskellPackages.callPackage ./pastebin.nix { }
