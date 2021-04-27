# Using Tailwind CSS

The `Ema.Helper.Tailwind` module provides a `layout` function that uses [twind](https://twind.dev/) shim that is used in the statically generated site, and otherwise uses Tailwind CSS from CDN in the dev server mode. This helper is for those that **use [Tailwind CSS](https://tailwindcss.com/) in conjunction with [blaze-html](https://hackage.haskell.org/package/blaze-html) DSL**.

To use the layout helper in your [render](guide/render.md) function:

```haskell
render :: Ema.CLI.Action -> MyModel -> MyRoute -> LByteString
render emaAction model route = do
  Tailwind.layout emaAction (H.title "My site") $ do 
    H.p "Hello world"
```

The very site you are viewing (ema.srid.ca) is a live demonstration of this helper.