{
  perSystem = { config, pkgs, lib, ... }:
    let
      tailwind = pkgs.haskellPackages.tailwind;
    in
    {
      devShells.default = pkgs.mkShell {
        name = "ema-template";
        meta.description = "ema-template development environment";
        packages = [
          tailwind
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
