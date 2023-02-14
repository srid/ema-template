{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-root.url = "github:srid/flake-root";
    proc-flake.url = "github:srid/proc-flake";
    mission-control.url = "github:Platonic-Systems/mission-control";

    nixpkgs-140774-workaround.url = "github:srid/nixpkgs-140774-workaround";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.proc-flake.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem = { self', config, inputs', pkgs, lib, ... }: {
        # "haskellProjects" comes from https://github.com/srid/haskell-flake
        haskellProjects.main = {
          imports = [
            inputs.nixpkgs-140774-workaround.haskellFlakeProjectModules.default
          ];
          devShell.tools = hp: {
            inherit (pkgs)
              treefmt
              nixpkgs-fmt;
            inherit (hp)
              cabal-fmt tailwind fourmolu;
          };
        };

        proc.groups.run.processes = {
          haskell.command = "ghcid";
          tailwind.command = "${lib.getExe pkgs.haskellPackages.tailwind} -w -o ./static/tailwind.css './src/**/*.hs'";
        };

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
            exec = config.proc.groups.run.package;
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
                  pushd ${self}
                  ${lib.getExe config.packages.main-ema-template} \
                    --base-url=${baseUrl} gen $out
                  ${lib.getExe pkgs.haskellPackages.tailwind} \
                    -o $out/tailwind.css 'src/**/*.hs'
                '';
          in
          {
            default = config.packages.main-ema-template;
            site = buildEmaSiteWithTailwind { baseUrl = "/"; };
            site-github = buildEmaSiteWithTailwind { baseUrl = "/ema-template/"; };
          };

        devShells.default = config.mission-control.installToDevShell config.devShells.main;
      };
    };
}
