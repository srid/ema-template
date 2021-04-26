# Ema Template

Use this repository to bootstrap your [Ema](https://ema.srid.ca/) static site. It currently acts as the source for Ema documentation (at ema.srid.ca) itself.

## Getting Started

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix-shell --run haskell-language-server` to sanity check your environment 
- Open as folder in Visual Studio Code
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` in terminal, to launch the Ema dev server.

## Tips

- As a first step to using template, change the project name in .cabal, .flake and hie.yaml files.
- To change the port, see bin/run
- To change the CLI arguments, see .ghcid
- To update Ema to latest Git revision, run `nix flake lock --update-input ema`
- To add/remove Haskell dependencies, see the .cabal file. If a dependency is unavailable in nixpkgs, you can override it (to point to say a Git repo) in the `overrides` attribute of flake.nix.