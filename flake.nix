{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus";

  outputs = { self, nixpkgs, flake-utils-plus }@inputs:
    let
      utils = flake-utils-plus.lib;
      lib = nixpkgs.lib;
    in
    utils.mkFlake {
      inherit self inputs;
      sharedOverlays = lib.attrValues self.overlays;
      overlays = import ./overlays;
      outputsBuilder = channels:
        let pkgs = channels.nixpkgs;
        in {
          packages.default = pkgs.callPackage ./default.nix { };
          devShells.default = pkgs.callPackage ./shell.nix { };
        };
    };
}
