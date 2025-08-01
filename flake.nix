{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.flake = false;

    ema.url = "github:srid/ema";
    ema.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        ./nix/modules/flake/haskell.nix
        ./nix/modules/flake/devshell.nix
        ./nix/modules/flake/pre-commit.nix
        ./nix/modules/flake/packages.nix
      ];
    };
}
