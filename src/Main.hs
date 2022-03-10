module Main where

import Ema
import Site.MarkdownSite
import Site.StaticSite 

main :: IO ()
main =
  -- TODO: static site
  void $ runSite @MarkdownRoute ()
