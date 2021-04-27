# Converting Markdown

There are quite a few packages to convert Markdown to HTML,

- [Pandoc](https://hackage.haskell.org/package/pandoc) -- [Supports formats other than Markdown]{.item-intro}
- [commonmark-hs](https://github.com/jgm/commonmark-hs) -- [Lightweight parser by the same author of PandoA]{.item-intro}
- [mmark](https://github.com/mmarkdown/mmark) -- [*Strict* Markdown parser]{.item-intro}

This very site uses `commonmark-hs` to parse and render Markdown. It also provides its own renderer of the Pandoc AST (which `commonmark-pandoc` provides) to be able to customize the CSS styling of elements. Consult [the source code](https://github.com/srid/ema-docs/blob/master/src/Main.hs) for details.

Note that with Ema you can get [hot reload](concepts/hot-reload.md) support for your Markdown files using [filesystem notifications](guide/helpers/filesystem.md).