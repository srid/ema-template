{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];
  perSystem = { config, pkgs, lib, ... }:
    let
      tailwindcss = lib.getExe pkgs.tailwindcss;
      buildEmaSiteWithTailwind = { baseUrl }:
        pkgs.runCommand "site"
          {
            LANG = "C.UTF-8";
            LC_ALL = "C.UTF-8";
          }
          ''
            mkdir -p $out
            cd ${inputs.self}/ema-template
            ${lib.getExe config.packages.ema-template} \
              --base-url=${baseUrl} gen $out
            rm -f $out/tailwind.css
            ${tailwindcss} \
              --content 'src/**/*.hs' -o $out/tailwind.css --minify
          '';
    in
    {
      process-compose."ema-tailwind-run" = {
        cli.environment.PC_DISABLE_TUI = true;
        settings = {
          processes = {
            haskell.command = "cd ./ema-template && ghcid";
            tailwind = {
              command = "cd ./ema-template && ${tailwindcss} --content './src/**/*.hs' -o ./static/tailwind.css --watch";
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
