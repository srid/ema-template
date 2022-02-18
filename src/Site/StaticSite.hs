module Site.StaticSite (staticSite) where

import Data.Text qualified as T
import Ema
import Ema.Route (unsafeMkRouteEncoder)

-- | A site that serves ./static statically.
staticSite :: Site () FilePath
staticSite =
  Site
    { siteName = "./static",
      siteRender = \_act _enc () -> Ema.AssetStatic,
      siteModelRunner = constModal (),
      siteRouteEncoder = staticRouteEncoder
    }

staticRouteEncoder :: RouteEncoder FilePath ()
staticRouteEncoder = unsafeMkRouteEncoder (const encodeRoute) (const decodeRoute) (const allRoutes)
  where
    encodeRoute = id
    decodeRoute fp = do
      guard $ "static/" `T.isPrefixOf` toText fp || fp == "static"
      pure fp
    -- Routes to write when generating the static site.
    allRoutes =
      ["static"]
