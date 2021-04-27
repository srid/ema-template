# Slug

In an _URL_ like `/foo/bar`, there are two _slugs_: "foo" and "bar". Thus, URLs in a static site can be represented by a _list_ of slugs. 

```haskell
import Ema (Slug)

type URL = [Slug]
```

When you defined [route](guide/routes.md) encoders and decodes (via [Ema class instance](guide/class.md)) you are writing functions that converts back and forth between your route type and `[Slug]`. These functions are ultimately used to determine the *filename* of the statically generated HTML (i.e., `./foo/bar.html`) as well as the linking URL in the rendered HTML (i.e., `/foo/bar`).