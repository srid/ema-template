# Using Markdown

There are quite a few packages to convert Markdown to HTML,

- [Pandoc](https://hackage.haskell.org/package/pandoc) -- [Supports formats other than Markdown]{.item-intro}
- [commonmark-hs](https://github.com/jgm/commonmark-hs) -- [Lightweight parser by the same author of Pandoc]{.item-intro}
- [mmark](https://github.com/mmark-md/mmark) -- [*Strict* Markdown parser]{.item-intro}

This very site uses `commonmark-hs` to parse Markdown, and `commonmark-pandoc` to parse it as Pandoc AST. Furthermore it provides its own renderer of the Pandoc AST, to be able to customize the CSS styling of individual AST nodes. Consult [the source code](https://github.com/srid/ema-docs/blob/master/src/Main.hs) for details.

Note that with Ema you can get [hot reload](concepts/hot-reload.md) support for your Markdown files using [filesystem notifications](guide/helpers/filesystem.md).