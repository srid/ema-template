{
  description = "Ema documentation source";
  inputs = {
    ema.url = "github:srid/ema/multisite";
    nixpkgs.follows = "ema/nixpkgs";
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
    flake-utils.follows = "ema/flake-utils";
    flake-compat.follows = "ema/flake-compat";

    pathtree.url = "github:srid/pathtree";
    pathtree.inputs.nixpkgs.follows = "ema/nixpkgs";
    commonmark-simple.url = "github:srid/commonmark-simple";
    commonmark-simple.inputs.nixpkgs.follows = "ema/nixpkgs";
    url-slug.url = "github:srid/url-slug";
    url-slug.inputs.nixpkgs.follows = "ema/nixpkgs";
    unionmount.url = "github:srid/unionmount/multisite";
    unionmount.inputs.nixpkgs.follows = "ema/nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        name = "ema-template";
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
        hp = pkgs.haskellPackages;
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
        project = returnShellEnv:
          hp.developPackage {
            inherit returnShellEnv name;
            root = pkgs.lib.cleanSourceWith { inherit filter name; src = ./.; };
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              ema = inputs.ema.defaultPackage.${system};
              tailwind = tailwind-haskell;
              pathtree = inputs.pathtree.defaultPackage.${system};
              commonmark-simple = inputs.commonmark-simple.defaultPackage.${system};
              url-slug = inputs.url-slug.defaultPackage.${system};
              unionmount = inputs.unionmount.defaultPackage.${system};
              relude = self.relude_1_0_0_1;
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with hp; [
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
