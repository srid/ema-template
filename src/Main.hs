{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.Route.Generic
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Generics.SOP qualified as SOP
import Optics.Core (Prism', (%))
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Route_Html HtmlRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithModel Model
             , WithSubRoutes '[HtmlRoute, StaticRoute]
             ]
        )

type StaticRoute = SR.StaticRoute "static"

data HtmlRoute
  = HtmlRoute_Index
  | HtmlRoute_About
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            HtmlRoute
            '[ -- Note: On GHC 9.2, WithSubRoutes is automatically determined
               WithSubRoutes
                '[ FileRoute "index.html"
                 , FileRoute "about.html"
                 ]
             ]
        )

data Model = Model
  { modelStatic :: SR.Model
  }
  deriving stock (Eq, Show, Generic)

instance EmaSite Route where
  siteInput cliAct () = do
    staticRouteDyn <- siteInput @StaticRoute cliAct ()
    pure $ Model <$> staticRouteDyn
  siteOutput rp m = \case
    Route_Html r ->
      Ema.AssetGenerated Ema.Html $ renderHtmlRoute rp m r
    Route_Static r ->
      siteOutput (rp % (_As @"Route_Static")) (modelStatic m) r

renderHtmlRoute :: Prism' FilePath Route -> Model -> HtmlRoute -> LByteString
renderHtmlRoute rp m r = do
  RU.renderHtml $ do
    H.docType
    H.html ! A.lang "en" $ do
      H.head $ do
        renderHead rp m r
      H.body $ do
        renderBody rp m r

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderHead rp model r = do
  H.meta ! A.charset "UTF-8"
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title $ H.toHtml $ routeTitle r <> " - Ema Template"
  H.base ! A.href "/"
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderBody rp model r = do
  H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
    renderNavbar rp r
    H.h1 ! A.class_ "text-3xl font-bold" $ H.toHtml $ routeTitle r
    case r of
      HtmlRoute_Index -> do
        "You are on the index page. Want to see "
        routeLink rp HtmlRoute_About "About"
        "?"
      HtmlRoute_About -> do
        "You are on the about page."
    H.img ! A.src (staticRouteUrl rp model "logo.svg") ! A.class_ "py-4 w-32" ! A.alt "Ema Logo"

renderNavbar :: Prism' FilePath Route -> HtmlRoute -> H.Html
renderNavbar rp currentRoute =
  H.nav ! A.class_ "w-full text-xl font-bold flex space-x-4  mb-4" $ do
    forM_ (universe @HtmlRoute) $ \r ->
      let extraClass = if r == currentRoute then "bg-rose-400 text-white" else "text-gray-700"
       in H.a ! A.href (H.toValue $ routeUrl rp $ Route_Html r)
            ! A.class_ ("rounded p-2 " <> extraClass)
            $ H.toHtml $ routeTitle r

routeTitle :: HtmlRoute -> Text
routeTitle r = case r of
  HtmlRoute_Index -> "Home"
  HtmlRoute_About -> "About"

routeLink :: Prism' FilePath Route -> HtmlRoute -> H.Html -> H.Html
routeLink rp r =
  H.a ! A.href (H.toValue $ routeUrl rp $ Route_Html r)
    ! A.class_ "text-rose-400"

-- | Link to a file under ./static
staticRouteUrl :: IsString r => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (rp % (_As @"Route_Static")) (modelStatic m)

main :: IO ()
main = Ema.runSite_ @Route ()