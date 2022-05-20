{-# LANGUAGE DeriveAnyClass #-}

{- | Ema app to serve static files without any dynamic generation involved.

 Usually you want to combine this with your real site.
-}
module Site.StaticSite (StaticPath) where

import Data.SOP
import Data.Text qualified as T
import Ema
import Ema.Route.Encoder (mkRouteEncoder)
import Optics.Core (prism')
import Prelude hiding (Generic)

-- | Relative path to the static directory (or file).
newtype StaticPath = StaticPath {unStaticPath :: FilePath}
  deriving stock (Show, Eq)

instance IsRoute StaticPath where
  type RouteModel StaticPath = NP I '[]
  routeEncoder = mkRouteEncoder $ const $ prism' unStaticPath parseRoute
    where
      parseRoute fp = do
        guard $ "static/" `T.isPrefixOf` toText fp || fp == "static"
        pure $ StaticPath fp
  allRoutes Nil =
    [StaticPath "static"]

instance EmaSite StaticPath where
  siteOutput _ _ (StaticPath fp) =
    Ema.AssetStatic fp
