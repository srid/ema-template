{
  description = "Ema template project";
  inputs = {
    ema.url = "github:srid/ema/multisite";
    nixpkgs.follows = "ema/nixpkgs";
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
    flake-utils.follows = "ema/flake-utils";
    flake-compat.follows = "ema/flake-compat";

    pathtree.url = "github:srid/pathtree";
    pathtree.inputs.nixpkgs.follows = "ema/nixpkgs";
    unionmount.url = "github:srid/unionmount/master";
    unionmount.inputs.nixpkgs.follows = "ema/nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        name = "ema-template";
        overlays = [ ];
        pkgs = nixpkgs.legacyPackages.${system};
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
              path-tree = self.callCabal2nix "path-tree" inputs.pathtree { };
              unionmount = self.callCabal2nix "unionmount" inputs.unionmount { };
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
