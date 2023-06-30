{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    flake-root.url = "github:srid/flake-root";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    mission-control.url = "github:Platonic-Systems/mission-control";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.process-compose-flake.flakeModule
        inputs.mission-control.flakeModule
        inputs.treefmt-nix.flakeModule
      ];
      perSystem = { self', config, inputs', pkgs, lib, ... }: {
        # "haskellProjects" comes from https://github.com/srid/haskell-flake
        haskellProjects.default = {
          devShell.tools = hp: {
            inherit (hp) tailwind;
          };
          autoWire = [ "packages" "apps" "checks" ];
        };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        # From https://github.com/Platonic-Systems/process-compose-flake
        process-compose.run = {
          tui = false;
          settings.processes = {
            haskell.command = "ghcid";
            tailwind.command = "${lib.getExe pkgs.haskellPackages.tailwind} -w -o ./static/tailwind.css './src/**/*.hs'";
          };
        };

        # From https://github.com/Platonic-Systems/mission-control
        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
          };
          fmt = {
            description = "Auto-format the source tree";
            exec = config.treefmt.build.wrapper;
          };
          run = {
            description = "Run the dev server (ghcid + tailwind)";
            exec = config.process-compose.run.outputs.package;
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
                  ${lib.getExe pkgs.haskellPackages.tailwind} \
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
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.treefmt.build.devShell
            config.mission-control.devShell
            config.flake-root.devShell
          ];
        };
      };
    };
}
