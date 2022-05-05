{
  description = "Ema template app";
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
          project = returnShellEnv:
            hp.developPackage {
              inherit returnShellEnv name;
              root = ./.;
              withHoogle = false;
              overrides = self: super: with pkgs.haskell.lib; {
                ema = inputs.ema.defaultPackage.${system};
                tailwind = tailwind-haskell;
                path-tree = self.callCabal2nix "path-tree" inputs.pathtree { };
                unionmount = self.callCabal2nix "unionmount" inputs.unionmount { };
              };
              modifier = drv:
                let inherit (pkgs.haskell.lib) addBuildTools;
                in
                pipe drv
                  [
                    # Transform the Haskell derivation (`drv`) here.
                    (flip addBuildTools
                      (optionals returnShellEnv shellDeps))
                  ];
            };
        in
        {
          # Used by `nix build` & `nix run`
          defaultPackage = project false;

          # Used by `nix develop`
          devShell = project true;
        }) // {
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
