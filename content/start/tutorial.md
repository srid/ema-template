# Creating your first site in Ema

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