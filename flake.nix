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
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.process-compose-flake.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem = { self', config, inputs', pkgs, lib, ... }: {
        # "haskellProjects" comes from https://github.com/srid/haskell-flake
        haskellProjects.default = {
          devShell.tools = hp: {
            inherit (pkgs)
              treefmt
              nixpkgs-fmt;
            inherit (hp)
              cabal-fmt tailwind fourmolu;
          };
          autoWire = [ "packages" "apps" "checks" ];
        };

        # From https://github.com/srid/proc-flake
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
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Auto-format the source tree";
            exec = "treefmt";
            category = "Dev Tools";
          };
          run = {
            description = "Run the dev server (ghcid + tailwind)";
            exec = config.process-compose.run.outputs.package;
            category = "Primary";
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
            config.mission-control.devShell
            config.haskellProjects.default.outputs.devShell
          ];
        };
      };
    };
}
