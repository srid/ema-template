{
  perSystem = { config, pkgs, lib, ... }:
    {
      devShells.default = pkgs.mkShell {
        name = "ema-template";
        meta.description = "ema-template development environment";
        packages = [
          pkgs.tailwindcss
          pkgs.just
          pkgs.nixd
        ];
        inputsFrom = [
          config.haskellProjects.default.outputs.devShell
          config.pre-commit.devShell
        ];
      };
    };
}
