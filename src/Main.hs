{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Default (def)
import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.CLI qualified
import Ema.Route.Generic.TH
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Lucid
import Optics.Core (Prism', (%))
import Options.Applicative

data Model = Model
  { modelBaseUrl :: Text
  , modelStatic :: SR.Model
  }
  deriving stock (Eq, Show, Generic)

data HtmlRoute
  = HtmlRoute_Index
  | HtmlRoute_About
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

deriveGeneric ''HtmlRoute
deriveIsRoute ''HtmlRoute [t|'[]|]

type StaticRoute = SR.StaticRoute "static"

data Route
  = Route_Html HtmlRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord, Generic)

deriveGeneric ''Route
deriveIsRoute
  ''Route
  [t|
    [ -- To render a `Route` we need `Model`
      WithModel Model
    , -- Override default sub-route encoding (to avoid the folder prefix in encoded URLs)
      WithSubRoutes [HtmlRoute, StaticRoute]
    ]
    |]

instance EmaSite Route where
  type SiteArg Route = CliArgs
  siteInput cliAct args = do
    staticRouteDyn <- siteInput @StaticRoute cliAct ()
    pure $ Model (cliArgsBaseUrl args) <$> staticRouteDyn
  siteOutput rp m = \case
    Route_Html r ->
      pure $ Ema.AssetGenerated Ema.Html $ renderHtmlRoute rp m r
    Route_Static r ->
      siteOutput (rp % (_As @"Route_Static")) (modelStatic m) r

renderHtmlRoute :: Prism' FilePath Route -> Model -> HtmlRoute -> LByteString
renderHtmlRoute rp m r = do
  renderBS $ doctypehtml_ $ do
    head_ $ renderHead rp m r
    body_ [class_ "bg-gray-50"] $ renderBody rp m r

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> Html ()
renderHead rp model r = do
  meta_ [charset_ "UTF-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  title_ $ toHtml $ routeTitle r <> " - Ema Template"
  base_ [href_ $ modelBaseUrl model]
  link_ [rel_ "stylesheet", href_ $ staticRouteUrl rp model "tailwind.css"]

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> Html ()
renderBody rp model r = do
  div_ [class_ "container mx-auto mt-8 p-4 max-w-prose border-2 bg-white rounded-lg shadow"] $ do
    renderNavbar rp r
    h1_ [class_ "text-3xl font-bold"] $ toHtml $ routeTitle r
    case r of
      HtmlRoute_Index -> do
        "You are on the index page. Want to see "
        routeLink rp HtmlRoute_About "About"
        "?"
      HtmlRoute_About -> do
        "You are on the about page."
    a_ [href_ $ staticRouteUrl rp model "logo.svg", target_ "_blank"] $ do
      img_ [src_ $ staticRouteUrl rp model "logo.svg", class_ "py-4 w-32", alt_ "Ema Logo"]

renderNavbar :: Prism' FilePath Route -> HtmlRoute -> Html ()
renderNavbar rp currentRoute =
  nav_ [class_ "w-full text-xl font-bold flex space-x-4  mb-4"] $ do
    forM_ (universe @HtmlRoute) $ \r ->
      let extraClass = if r == currentRoute then "bg-rose-400 text-white" else "text-gray-700"
       in a_
            [ href_ $ routeUrl rp $ Route_Html r
            , class_ $ "rounded p-2 " <> extraClass
            ]
            $ toHtml
            $ routeTitle r

routeTitle :: HtmlRoute -> Text
routeTitle r = case r of
  HtmlRoute_Index -> "Home"
  HtmlRoute_About -> "About"

routeLink :: Prism' FilePath Route -> HtmlRoute -> Html () -> Html ()
routeLink rp r =
  a_
    [ href_ $ routeUrl rp $ Route_Html r
    , class_ "text-rose-400"
    ]

-- | Link to a file under ./static
staticRouteUrl :: (IsString r) => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (rp % (_As @"Route_Static")) (modelStatic m)

-- CLI argument handling
-- ---------------------

data CliArgs = CliArgs
  { cliArgsBaseUrl :: Text
  , cliArgsEmaCli :: Ema.CLI.Cli
  }
  deriving stock (Eq, Show)

parseCliArgs :: IO CliArgs
parseCliArgs =
  execParser $ parserInfo cliParser
  where
    cliParser :: Parser CliArgs
    cliParser =
      CliArgs
        <$> option str (long "base-url" <> metavar "BASE_URL" <> help "Base URL to use in <base>" <> value "/")
        <*> Ema.CLI.cliParser
    parserInfo :: Parser a -> ParserInfo a
    parserInfo p =
      info
        (versionOption <*> p <**> helper)
        ( fullDesc
            <> progDesc "ema-template: TODO"
            <> header "ema-template"
        )
      where
        versionOption = infoOption "0.1" (long "version" <> help "Show version")

-- Main entrypoint
-- ---------------

main :: IO ()
main = do
  cliArgs <- parseCliArgs
  let cfg = SiteConfig (cliArgsEmaCli cliArgs) def
  void $ Ema.runSiteWith @Route cfg cliArgs
