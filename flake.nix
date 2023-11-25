{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    flake-root.url = "github:srid/flake-root";
    proc-flake.url = "github:srid/proc-flake";
    mission-control.url = "github:Platonic-Systems/mission-control";
    treefmt-nix.url = "github:numtide/treefmt-nix";

    ema.url = "github:srid/ema";
    ema.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.proc-flake.flakeModule
        inputs.mission-control.flakeModule
        inputs.treefmt-nix.flakeModule
      ];
      perSystem = { self', config, inputs', pkgs, lib, ... }:
        let
          tailwind = config.haskellProjects.default.outputs.finalPackages.tailwind;
        in
        {
          # "haskellProjects" comes from https://github.com/srid/haskell-flake
          haskellProjects.default = {
            imports = [
              inputs.ema.haskellFlakeProjectModules.output
            ];
            settings = {
              tailwind = {
                broken = false;
                jailbreak = true;
              };
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
          proc.groups.run = {
            processes = {
              haskell.command = "ghcid";
              tailwind.command = "${lib.getExe tailwind} -w -o ./static/tailwind.css './src/**/*.hs'";
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
              exec = config.proc.groups.run.package;
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
            packages = [
              tailwind
            ];
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
