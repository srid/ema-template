module Main where

import Ema
import Generics.SOP
import Site.MarkdownSite
import Site.StaticSite

main :: IO ()
main =
  -- TODO: static site
  void $ runSite @(NS I '[MarkdownRoute, StaticFile]) ()

instance IsRoute (NS I rs) where
  type RouteModel (NS I rs) = NP I rs -- ms
  routeEncoder = undefined

instance HasModel (NS I rs) where
  type ModelInput (NS I rs) = ()
  modelDynamic _ _ () = do
    undefined

instance CanRender (NS I rs) where
  routeAsset enc m r =
    undefined

instance CanGenerate (NS I rs) where
  generatableRoutes _ =
    undefined