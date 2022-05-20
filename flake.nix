{
  description = "Ema template app";
  inputs = {
    ema.url = "github:srid/ema/multisite";
    nixpkgs.follows = "ema/nixpkgs";
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
    flake-utils.follows = "ema/flake-utils";
    flake-compat.follows = "ema/flake-compat";

    unionmount.url = "github:srid/unionmount/master";
    unionmount.inputs.nixpkgs.follows = "ema/nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          name = "ema-template";
          overlays = [ ];
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (pkgs.lib.trivial) pipe flip;
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
                unionmount = self.callCabal2nix "unionmount" inputs.unionmount { };
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
        }) // {
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
