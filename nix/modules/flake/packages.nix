{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];
  perSystem = { config, pkgs, lib, ... }:
    let
      tailwind = pkgs.haskellPackages.tailwind;
      buildEmaSiteWithTailwind = { baseUrl }:
        pkgs.runCommand "site"
          {
             LANG = "C.UTF-8";
             LC_ALL = "C.UTF-8";
             GHC_IO_ENCODING = "utf-8";
          }
          ''
            mkdir -p $out
            export LC_ALL
            export LANG
            export GHC_IO_ENCODING
            pushd ${inputs.self}/ema-template
            ${lib.getExe config.packages.ema-template} \
              --base-url=${baseUrl} gen $out
            ${lib.getExe tailwind} \
              -o $out/tailwind.css 'src/**/*.hs'
          '';
    in
    {
      process-compose."ema-tailwind-run" = {
        cli.environment.PC_DISABLE_TUI = true;
        settings = {
          processes = {
            haskell.command = "cd ./ema-template && ghcid";
            tailwind = {
              command = "cd ./ema-template && ${lib.getExe tailwind} -w -o ./static/tailwind.css './src/**/*.hs'";
              is_tty = true;
            };
          };
        };
      };

      packages = {
        site = buildEmaSiteWithTailwind { baseUrl = "/"; };
        site-github = buildEmaSiteWithTailwind { baseUrl = "/ema-template/"; };
      };
    };
}
