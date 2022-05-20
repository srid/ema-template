{- | Ema app to serve static files without any dynamic generation involved.

 Usually you want to combine this with your real site.
-}
module Site.StaticSite (StaticPath) where

import Data.Text qualified as T
import Ema
import Ema.Route.Encoder (mkRouteEncoder)
import Optics.Core (prism')
import System.FilePath (splitFileName, (</>))

-- | Relative path to a file or directory inside `StaticDir`.
newtype StaticPath = StaticPath {unStaticPath :: FilePath}
  deriving stock (Show, Eq)

data StaticDir = StaticDir
  { _staticDirBaseDir :: FilePath
  , _staticDirName :: String
  }
  deriving stock (Show, Eq)

instance IsRoute StaticPath where
  type RouteModel StaticPath = StaticDir
  routeEncoder = mkRouteEncoder $ \m -> prism' unStaticPath (parseRoute m)
    where
      parseRoute m fp = do
        let dir = _staticDirName m
        guard $ (toText dir <> "/") `T.isPrefixOf` toText fp || fp == dir
        pure $ StaticPath fp
  allRoutes m =
    [StaticPath $ _staticDirName m]

instance EmaSite StaticPath where
  type SiteArg StaticPath = FilePath -- The directory containing static files
  siteInput _ _ staticDir = do
    let (base, dir) = splitFileName staticDir
    pure $ pure $ StaticDir base dir
  siteOutput _ m (StaticPath fp) =
    Ema.AssetStatic $ _staticDirBaseDir m </> fp
