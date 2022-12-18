{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-root.url = "github:srid/flake-root";
    proc-flake.url = "github:srid/proc-flake";
    mission-control.url = "github:Platonic-Systems/mission-control";

    # Haskell overrides
    ema.url = "github:srid/ema";
    ema.flake = false;
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
          packages.ema-template.root = ./.;
          haskellPackages = pkgs.haskell.packages.ghc924;
          buildTools = hp: {
            inherit (pkgs)
              treefmt
              nixpkgs-fmt;
            inherit (pkgs.haskellPackages)
              cabal-fmt tailwind;
            inherit (hp)
              fourmolu;

            # https://github.com/NixOS/nixpkgs/issues/140774 reoccurs in GHC 9.2
            ghcid = pkgs.haskell.lib.overrideCabal hp.ghcid (drv: {
              enableSeparateBinOutput = false;
            });
          };
          source-overrides = {
            ema = inputs.ema + /ema;
            ema-generics = inputs.ema + /ema-generics;
            ema-extra = inputs.ema + /ema-extra;
          };
          overrides = self: super: with pkgs.haskell.lib; {
            type-errors-pretty = dontCheck (doJailbreak super.type-errors-pretty);
            relude = dontCheck (self.callHackage "relude" "1.1.0.0" { }); # 1.1 not in nixpkgs yet 
            retry = dontCheck super.retry; # For GHC 9.2.
            streaming-commons = dontCheck super.streaming-commons; # Fails on darwin
            http2 = dontCheck super.http2; # Fails on darwin
            hls-explicit-fixity-plugin = dontCheck super.hls-explicit-fixity-plugin;
          };
        };
        proc.groups.run.processes = {
          haskell.command = "${lib.getExe pkgs.haskellPackages.ghcid}";
          tailwind.command = "${lib.getExe pkgs.haskellPackages.tailwind} -w -o ./static/tailwind.css './src/**/*.hs'";
        };
        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            command = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            command = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Auto-format the source tree";
            command = "treefmt";
            category = "Dev Tools";
          };
          run = {
            description = "Run the project with ghcid auto-recompile";
            package = config.proc.groups.run.package;
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
                  ${lib.getExe config.packages.default} \
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
