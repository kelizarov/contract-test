{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { allowUnfreePredicate = (import ./nix/lib/unfree.nix).unfreePredicate; }
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, isInFlake ? false
, haskellNix ? import sources."haskell.nix" {
    sourcesOverride = {
      hackage = sources."hackage.nix";
      stackage = sources."stackage.nix";
    };
  }
, packages ? import ./nix { inherit system sources crossSystem config sourcesOverride haskellNix isInFlake checkMaterialization enableHaskellProfiling; }
  # An explicit git rev to use, passed when we are in Hydra
  # Whether to check that the pinned shas for haskell.nix are correct. We want this to be
  # false, generally, since it does more work, but we set it to true in the CI
, checkMaterialization ? false
  # Whether to build our Haskell packages (and their dependencies) with profiling enabled.
, enableHaskellProfiling ? false
}:
let
  inherit (packages) pkgs plutus sources;
  inherit (pkgs) lib haskell-nix;
  inherit (plutus) haskell agdaPackages;
  inherit (plutus) easyPS sphinxcontrib-haddock;
in
rec {
  inherit pkgs plutus;

  webCommon = pkgs.callPackage ../plutus/web-common { inherit (plutus.lib) gitignore-nix; };
  webCommonPlutus = pkgs.callPackage ../plutus/web-common-plutus { inherit (plutus.lib) gitignore-nix; };
  webCommonMarlowe = pkgs.callPackage ../plutus/web-common-marlowe { inherit (plutus.lib) gitignore-nix; };
  webCommonPlayground = pkgs.callPackage ../plutus/web-common-playground { inherit (plutus.lib) gitignore-nix; };

  plutus-playground = pkgs.recurseIntoAttrs rec {
    haddock = plutus.plutus-haddock-combined;

    inherit (pkgs.callPackage ../plutus/plutus-playground-client {
      inherit (plutus.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit haskell webCommon webCommonPlutus webCommonPlayground;
    }) client server generate-purescript start-backend;
  };

  plutus-pab = pkgs.recurseIntoAttrs (pkgs.callPackage ../plutus/plutus-pab-client {
    inherit (plutus.lib) buildPursPackage buildNodeModules gitignore-nix filterNpm;
    inherit haskell webCommon webCommonPlutus;
  });

  tests = import ./nix/tests/default.nix {
    inherit pkgs docs;
    inherit (plutus.lib) gitignore-nix;
    inherit (plutus) fixStylishHaskell fixPurty fixPngOptimization;
    inherit (pkgs) terraform;
    inherit plutus-playground plutus-pab;
    src = ../plutus/.;
  };

  docs = import ./nix/docs.nix { inherit pkgs plutus; };

  # This builds a vscode devcontainer that can be used with the plutus-starter project (or probably the plutus project itself).
  devcontainer = import ./nix/devcontainer/plutus-devcontainer.nix { inherit pkgs plutus; };
  build-and-push-devcontainer-script = import ./nix/devcontainer/deploy/default.nix { inherit pkgs plutus; };
}
