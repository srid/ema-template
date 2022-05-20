{- | Ema app to serve static files without any dynamic generation involved.

 Usually you want to combine this with your real site.
-}
module Site.StaticSite (StaticPath) where

import Data.Text qualified as T
import Ema
import Ema.Route.Encoder (mkRouteEncoder)
import Optics.Core (prism')

-- | Relative path to the static directory (or file).
newtype StaticPath = StaticPath {unStaticPath :: FilePath}
  deriving stock (Show, Eq)

instance IsRoute StaticPath where
  type RouteModel StaticPath = ()
  routeEncoder = mkRouteEncoder $ const $ prism' unStaticPath parseRoute
    where
      parseRoute fp = do
        guard $ "static/" `T.isPrefixOf` toText fp || fp == "static"
        pure $ StaticPath fp
  allRoutes () =
    [StaticPath "static"]

instance EmaSite StaticPath where
  siteInput _ _ () =
    pure $ pure ()
  siteOutput _ _ (StaticPath fp) =
    Ema.AssetStatic fp
