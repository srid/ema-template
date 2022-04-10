module Main where

import Ema
import Ema.Multi
import Generics.SOP
import Site.MarkdownSite
import Site.StaticSite

main :: IO ()
main =
  void $ runSite @(MultiRoute '[MarkdownRoute, StaticPath]) (I () :* I () :* Nil)
