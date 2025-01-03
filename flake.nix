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
        inputs.haskell-flake.flakeModule
        inputs.process-compose-flake.flakeModule
        inputs.fourmolu-nix.flakeModule
        (inputs.git-hooks + /flake-module.nix)
      ];
      perSystem = { config, pkgs, lib, ... }:
        let
          tailwind = pkgs.haskellPackages.tailwind;
        in
        {
          # "haskellProjects" comes from https://github.com/srid/haskell-flake
          haskellProjects.default = {
            imports = [
              inputs.ema.haskellFlakeProjectModules.output
            ];
            autoWire = [ "packages" "apps" "checks" ];
          };

          pre-commit.settings = {
            hooks = {
              nixpkgs-fmt.enable = true;
              cabal-fmt.enable = true;
              fourmolu = {
                enable = true;
                package = config.fourmolu.wrapper;
              };
              hlint.enable = true;
            };
          };

          fourmolu.settings = {
            indentation = 2;
            comma-style = "leading";
            record-brace-space = true;
            indent-wheres = true;
            import-export-style = "diff-friendly";
            respectful = true;
            haddock-style = "multi-line";
            newlines-between-decls = 1;
            extensions = [ "ImportQualifiedPost" ];
          };

          process-compose."ema-tailwind-run" = {
            cli.environment.PC_DISABLE_TUI = true;
            settings = {
              processes = {
                haskell.command = "ghcid";
                tailwind = {
                  command = "${lib.getExe tailwind} -w -o ./static/tailwind.css './src/**/*.hs'";
                  is_tty = true;
                };
              };
            };
          };

          packages =
            let
              buildEmaSiteWithTailwind = { baseUrl }:
                pkgs.runCommand "site"
                  { }
                  ''
                    mkdir -p $out
                    pushd ${inputs.self}
                    ${lib.getExe config.packages.ema-template} \
                      --base-url=${baseUrl} gen $out
                    ${lib.getExe tailwind} \
                      -o $out/tailwind.css 'src/**/*.hs'
                  '';
            in
            {
              default = config.packages.ema-template;
              site = buildEmaSiteWithTailwind { baseUrl = "/"; };
              site-github = buildEmaSiteWithTailwind { baseUrl = "/ema-template/"; };
            };

          devShells.default = pkgs.mkShell {
            name = "ema-template";
            meta.description = "ema-template development environment";
            packages = [
              tailwind
              pkgs.just
              pkgs.nixd
            ];
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
              config.pre-commit.devShell
            ];
          };
        };
    };
}
