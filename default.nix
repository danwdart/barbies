{
  nixpkgs ? import <unstable> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {},
  compiler ? "ghc922"
}:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      barbies = lib.dontHaddock (self.callCabal2nix "barbies" (gitignore ./.) {});
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.barbies
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find -type f); do krank $i; done
    '';
    buildInputs = tools.defaultBuildTools;
    # withHoogle = false;
  };
  exe = lib.justStaticExecutables (myHaskellPackages.barbies);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  barbies = myHaskellPackages.barbies;
}