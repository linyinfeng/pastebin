{
  pkgs ? import <nixpkgs> { },
}:

pkgs.haskellPackages.callPackage ./pastebin.nix { }
