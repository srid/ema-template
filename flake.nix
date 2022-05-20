{
  description = "Ema template app";
  inputs = {
    ema.url = "github:srid/ema/multisite";
    nixpkgs.follows = "ema/nixpkgs";
    flake-utils.follows = "ema/flake-utils";
    flake-compat.follows = "ema/flake-compat";

    # Haskell overrides
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        name = "ema-template";
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs.lib.lists) optionals;
        hp = pkgs.haskellPackages;
        tailwind-haskell = inputs.tailwind-haskell.defaultPackage.${system};
        shellDeps = with hp; [
          cabal-fmt
          cabal-install
          ghcid
          haskell-language-server
          fourmolu
          pkgs.nixpkgs-fmt
          tailwind-haskell
          pkgs.foreman
          pkgs.treefmt
        ];
        project =
          { returnShellEnv ? false
          , withHoogle ? false
          }:
          hp.developPackage {
            inherit returnShellEnv withHoogle name;
            root = ./.;
            overrides = self: super: with pkgs.haskell.lib; {
              ema = inputs.ema.defaultPackage.${system};
              tailwind = tailwind-haskell;
            };
            modifier = drv:
              pkgs.haskell.lib.overrideCabal drv (oa: {
                # All the Cabal-specific overrides go here.
                # For examples on what is possible, see:
                #   https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib/compose.nix
                buildTools = (oa.buildTools or [ ]) ++ optionals returnShellEnv shellDeps;
              });
          };
      in
      {
        # Used by `nix build` & `nix run`
        defaultPackage = project { };

        # Used by `nix develop`
        devShell = project { returnShellEnv = true; withHoogle = true; };
      }
    );
}
