# Getting Started

As first steps, perform the following before proceeding to the tutorial section below:

1. [Install Nix](https://nixos.org/download.html) (see [platform-specific notes here](https://neuron.zettel.page/install))
1. [Enable Flakes](https://nixos.wiki/wiki/Flakes)
1. Clone [the template repository](https://github.com/srid/ema-docs) locally
1. Run `bin/run` and access the site at <http://localhost:9001>

That should give you a preview of Ema site. Read the tutorial that follows to get acquainted with writing code using Ema.

## Tutorial

**TODO**

- Point to template (using helpers) first.
- Replace template Main.hs with very basic site
- Model, routes, view (similar to TEA)
- port, hot reload
- Generate static files

The simplest Ema app looks like this:

```haskell
main :: IO ()
main = do
  let name :: Text = "Ema"
  runEmaPure $ \_
    encodeUtf8 $ "<b>Hello</b>, from " <> name
```

{.last}
[Next]{.next}, checkout the [Guide](guide.md) series for information on specific topics.
