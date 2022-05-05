final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = self: super: {
      amazonka = self.callPackage ./amazonka.nix {};
      amazonka-core = self.callPackage ./amazonka-core.nix {};
      amazonka-test = self.callPackage ./amazonka-test.nix {};
      amazonka-s3 = self.callPackage ./amazonka-s3.nix {};
      amazonka-sts = self.callPackage ./amazonka-sts.nix {};
      amazonka-sso = self.callPackage ./amazonka-sso.nix {};
    };
  };
}
