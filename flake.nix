{
  description = "Ema documentation source";
  inputs = {
    ema.url = "github:srid/ema/master";
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
    # Use the nixpkgs used by the pinned ema.
    nixpkgs.follows = "ema/nixpkgs";
    flake-utils.follows = "ema/flake-utils";
    flake-compat.follows = "ema/flake-compat";

    pathtree.url = "github:srid/pathtree";
    pathtree.inputs.nixpkgs.follows = "ema/nixpkgs";
    commonmark-simple.url = "github:srid/commonmark-simple";
    commonmark-simple.inputs.nixpkgs.follows = "ema/nixpkgs";
    url-slug.url = "github:srid/url-slug";
    url-slug.inputs.nixpkgs.follows = "ema/nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        name = "ema-template";
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
        tailwind-haskell = inputs.tailwind-haskell.defaultPackage.${system};
        # Based on https://github.com/input-output-hk/daedalus/blob/develop/yarn2nix.nix#L58-L71
        filter = name: type:
          let
            baseName = baseNameOf (toString name);
            sansPrefix = pkgs.lib.removePrefix (toString ./.) name;
          in
          # Ignore these files when building source package
            !(
              baseName == "README.md" ||
              sansPrefix == "/bin" ||
              sansPrefix == "/content" ||
              sansPrefix == "/.github" ||
              sansPrefix == "/.vscode" ||
              sansPrefix == "/.ghcid"
            );
        m1MacHsBuildTools =
          pkgs.haskellPackages.override {
            overrides = self: super:
              let
                workaround140774 = hpkg: with pkgs.haskell.lib;
                  overrideCabal hpkg (drv: {
                    enableSeparateBinOutput = false;
                  });
              in
              {
                ghcid = workaround140774 super.ghcid;
                ormolu = workaround140774 super.ormolu;
              };
          };
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv name;
            root = pkgs.lib.cleanSourceWith { inherit filter name; src = ./.; };
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              ema = inputs.ema.defaultPackage.${system};
              tailwind = tailwind-haskell;
              pathtree = inputs.pathtree.defaultPackage.${system};
              commonmark-simple = inputs.commonmark-simple.defaultPackage.${system};
              url-slug = inputs.url-slug.defaultPackage.${system};
              # lvar = self.callCabal2nix "lvar" inputs.ema.inputs.lvar { }; # Until lvar gets into nixpkgs
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with (if system == "aarch64-darwin"
                then m1MacHsBuildTools
                else pkgs.haskellPackages); [
                  cabal-fmt
                  cabal-install
                  ghcid
                  haskell-language-server
                  ormolu
                  pkgs.nixpkgs-fmt
                  tailwind-haskell
                  pkgs.foreman
                ]);
          };
      in
      {
        # Used by `nix build` & `nix run`
        defaultPackage = project false;

        # Used by `nix develop`
        devShell = project true;
      });
}
