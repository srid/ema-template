{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { lib, config, ... }: {
    haskellProjects.default = {
      imports = [
        inputs.ema.haskellFlakeProjectModules.output
      ];
      projectRoot = builtins.toString (lib.fileset.toSource {
        root = root;
        fileset = lib.fileset.unions [
          (root + /ema-template)
          (root + /cabal.project)
          (root + /LICENSE)
        ];
      });
      autoWire = [ "packages" "apps" "checks" ];
    };

    packages.default = config.packages.ema-template;
  };
}
