module Main where

import Ema qualified
import Ema.Multi (MultiRoute)
import Generics.SOP (I (I), NP (Nil, (:*)))
import Site.MarkdownSite (MarkdownRoute)
import Site.StaticSite (StaticPath)

main :: IO ()
main =
  void $
    Ema.runSite
      @(MultiRoute '[MarkdownRoute, StaticPath])
      ( I "./content" -- `SiteArg` for `MarkdownRoute`
          :* I "./content/static" -- `SiteArg` for `StaticPath`
          :* Nil
      )
