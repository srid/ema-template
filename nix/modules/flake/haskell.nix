{ inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { config, ... }: {
    haskellProjects.default = {
      imports = [
        inputs.ema.haskellFlakeProjectModules.output
      ];
      autoWire = [ "packages" "apps" "checks" ];
    };

    packages.default = config.packages.ema-template;
  };
}
