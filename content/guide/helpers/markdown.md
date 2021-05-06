---
order: 3
---

# Using Markdown

There are quite a few packages to convert Markdown to HTML,

- [Pandoc](https://hackage.haskell.org/package/pandoc) -- [Supports formats other than Markdown]{.item-intro}
- [commonmark-hs](https://github.com/jgm/commonmark-hs) -- [Lightweight parser by the same author of Pandoc]{.item-intro}
- [mmark](https://github.com/mmark-md/mmark) -- [*Strict* Markdown parser]{.item-intro}

## Helper

Ema provides a helper to parse Markdown files with YAML frontmatter, using commonmark-hs.

```haskell
import qualified Ema.Helper.Markdowen as Markdown

-- This is usually a sum type
type Metadata = Map Text Text 

-- Returns `Either Text (Metadata, Pandoc)`
Markdown.parseMarkdownWithFrontMatter 
    @Metadata "test.md" "Hello *world*"
```

This very site uses this helper to parse Markdown files into Pandoc AST. Furthermore it provides its own renderer of the Pandoc AST, to be able to customize the CSS styling of individual AST nodes. Consult [the source code](https://github.com/srid/ema-docs/blob/master/src/Main.hs) for details.

Note that with Ema you can get [hot reload](concepts/hot-reload.md) support for your Markdown files using [filesystem notifications](guide/helpers/filesystem.md).
