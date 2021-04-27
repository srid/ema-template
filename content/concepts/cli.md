# CLI

Ema apps have a basic CLI argument structure that takes two kinds of input:

1. `-C <dir>`: specifies the "input directory" (current working directory by default)
2. `gen` subcommand: generates the static site, instead of starting up the dev server

The "input directory" is not used by Ema, but is meant to be used by your application. It, along with the "gen" subcommand (if used), is passed as the `Ema.CLI.Action` type to your `render` function. You can also use `runEmaWith` if you are manually handling the CLI arguments yourself.