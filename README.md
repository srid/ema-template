# Ema Template

A very simple example [Ema](https://ema.srid.ca/) site that is based on Blaze HTML & TailwindCSS 3. Use it to bootstrap your next static site using Ema.

The generated HTML site can be previewed here: https://srid.github.io/ema-template/

## Getting Started

To develop with full IDE support in Visual Studio Code, follow these steps:

- [Install Nix](https://nixos.asia/en/install)
  - And [setup direnv](https://nixos.asia/en/direnv)
- Run `nix develop -i -c haskell-language-server` to sanity check your environment 
- Open the repository [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the recommended extensions
    - Active the direnv environment
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `nix develop -c just run` in terminal (`just run` if direnv is already active), to launch the Ema dev server, and navigate to http://localhost:9001/

All but the final step need to be done only once. Check [the Ema tutorial](https://ema.srid.ca/tutorial) next.

## Note

- This project uses [relude](https://github.com/kowainik/relude) as its prelude, as well as Tailwind+Blaze as CSS utility and HTML DSL. Even though the author highly recommends them, you are of course free to swap them out for the library of your choice.
  - Tailwind CSS is compiled, alongside Ghcid, via foreman (see `flake.nix`)
- As a first step to using this template, rename your project using https://srid.ca/haskell-template/start#rename-the-project (use `ema-template` in place of `haskell-template`)
- Configuration:
  - To change the port (or the Ema CLI arguments, used by `nix develop -c just run`), see `./.ghcid` (if you leave out `--port` a random port will be used)
  - To add/remove Haskell dependencies, see https://community.flake.parts/haskell-flake/dependency
- To generate the site, run:
  ```sh
  nix build .#site
  # Alternatively:
  # > mkdir ../output 
  # > nix run . -- --base-url=/ gen ../output
  ```

## Non-Nix workflow

To use this repository without Nix (such as with plain Cabal or Stack) you need to have the following installed manually:

- ghcid
- [tailwind runner](https://hackage.haskell.org/package/tailwind) along with [tailwind CLI](https://tailwindcss.com/docs/installation)
- [foreman](http://ddollar.github.io/foreman/) (or one of its rewrites)
- Add a `Procfile`; see flake.nix to determine what should go in its `Procfile`

Once all the above are installed and setup, run `foreman start` to start the Ema live server.
