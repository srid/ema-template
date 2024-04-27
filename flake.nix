{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    flake-root.url = "github:srid/flake-root";
    proc-flake.url = "github:srid/proc-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";

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
        inputs.treefmt-nix.flakeModule
        inputs.fourmolu-nix.flakeModule
      ];
      perSystem = { self', config, inputs', pkgs, lib, ... }:
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

          # Auto formatters. This also adds a flake check to ensure that the
          # source tree was auto formatted.
          treefmt.config = {
            inherit (config.flake-root) projectRootFile;
            package = pkgs.treefmt;

            programs.fourmolu.enable = true;
            programs.nixpkgs-fmt.enable = true;
            programs.cabal-fmt.enable = true;
            programs.hlint.enable = true;

            # We use fourmolu
            programs.fourmolu.package = config.fourmolu.wrapper;
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

          # From https://github.com/srid/proc-flake
          proc.groups.ema-tailwind-run = {
            processes = {
              haskell.command = "ghcid";
              tailwind.command = "${lib.getExe tailwind} -w -o ./static/tailwind.css './src/**/*.hs'";
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
              config.proc.groups.ema-tailwind-run.package
            ];
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
              config.treefmt.build.devShell
              config.flake-root.devShell
            ];
          };
        };
    };
}
