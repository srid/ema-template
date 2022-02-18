module Main where

import Ema
import Site.MarkdownSite (markdownSite)
import Site.StaticSite (staticSite)

main :: IO ()
main =
  runSite_ $
    markdownSite
      `mergeSite` staticSite
