{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
        inputs.treefmt-nix.flakeModule
      ];
      perSystem =
        {
          config,
          self',
          pkgs,
          system,
          ...
        }:
        {
          packages = {
            pastebin = pkgs.callPackage ./. { };
            default = self'.packages.pastebin;
          };
          overlayAttrs = {
            inherit (self'.packages) pastebin;
          };
          checks = {
            inherit (self'.packages) pastebin;
          };
          treefmt = {
            projectRootFile = "flake.nix";
            programs = {
              nixfmt-rfc-style.enable = true;
              ormolu.enable = true;
              cabal-fmt.enable = true;
              prettier.enable = true;
            };
          };
          devShells.default = pkgs.callPackage ./shell.nix { };
        };
    };
}
