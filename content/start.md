# Getting Started

Ema is heavily a work in progress. Version 1.0 release is expected soon. If you wish to adopt it early for your projects, start from the WIP template repo [srid/ema-docs](https://github.com/srid/ema-docs). 

The simplest Ema app looks like this:

```haskell
main :: IO ()
main = do
  let name :: Text = "Ema"
  runEmaPure $ \_
    encodeUtf8 $ "<b>Hello</b>, from " <> name
```

{.last}
[Next]{.next}, we will [develop a basic site](start/tutorial.md) using Ema.