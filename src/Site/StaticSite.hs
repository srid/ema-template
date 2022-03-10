{-# LANGUAGE DeriveAnyClass #-}

module Site.StaticSite (StaticFile) where

import Data.Text qualified as T
import Ema
import Ema.Route.Encoder (unsafeMkRouteEncoder)
import Ema.Route.Generic (IsRoute (..))
import Prelude hiding (Generic)

newtype StaticFile = StaticFile {unStaticFile :: FilePath}
  deriving stock (Show, Eq)
  deriving anyclass (HasModel)

instance RenderAsset StaticFile where
  renderAsset _ _ (StaticFile fp) =
    Ema.AssetStatic fp

instance IsRoute StaticFile where
  type RouteModel StaticFile = ()
  mkRouteEncoder = unsafeMkRouteEncoder (const encodeRoute) (const decodeRoute) (const allRoutes)
    where
      encodeRoute = unStaticFile
      decodeRoute fp = do
        guard $ "static/" `T.isPrefixOf` toText fp || fp == "static"
        pure $ StaticFile fp
      -- Routes to write when generating the static site.
      allRoutes =
        [StaticFile "static"]
