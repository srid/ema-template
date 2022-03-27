{-# LANGUAGE DeriveAnyClass #-}

module Site.StaticSite (StaticFile) where

import Data.SOP
import Data.Text qualified as T
import Ema
import Ema.Route.Encoder
import Optics.Core (prism')
import Prelude hiding (Generic)

newtype StaticFile = StaticFile {unStaticFile :: FilePath}
  deriving stock (Show, Eq)
  deriving anyclass (HasModel)

instance CanRender StaticFile where
  routeAsset _ _ (StaticFile fp) =
    Ema.AssetStatic fp

instance IsRoute StaticFile where
  type RouteModel StaticFile = NP I '[]
  routeEncoder = mkRouteEncoder $ const $ prism' enc dec
    where
      enc = unStaticFile
      dec fp = do
        guard $ "static/" `T.isPrefixOf` toText fp || fp == "static"
        pure $ StaticFile fp

instance CanGenerate StaticFile where
  generatableRoutes Nil =
    [StaticFile "static"]
