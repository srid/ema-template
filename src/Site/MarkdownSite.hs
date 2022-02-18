{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Site.MarkdownSite (markdownSite) where

import Commonmark.Simple qualified as Commonmark
import Control.Exception (throw)
import Control.Monad.Logger
import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import Data.LVar (LVar)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Some (Some)
import Data.Text qualified as T
import Data.Tree (Tree (Node))
import Data.Tree.Path qualified as PathTree
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Ema
import Ema.CLI qualified
import Ema.Route (unsafeMkRouteEncoder)
import NeatInterpolation (text)
import Network.URI.Slug (Slug)
import Network.URI.Slug qualified as Slug
import Shower qualified
import System.FilePath (splitExtension, splitPath)
import System.UnionMount qualified as UnionMount
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Walk qualified as W
import UnliftIO (MonadUnliftIO)

-- ------------------------
-- Our site route
-- ------------------------

-- | Represents the relative path to a source (.md) file under some directory.
--
-- We will reuse this in our site route type to refer to the corresponding .html.
--
-- If you are using this repo as a template, you might want to use an ADT as
-- route (eg: data Route = Index | About)
newtype MarkdownRoute = MarkdownRoute {unMarkdownRoute :: NonEmpty Slug}
  deriving newtype (Eq, Ord, Show)

newtype BadRoute = BadRoute MarkdownRoute
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Represents the top-level index.md
indexMarkdownRoute :: MarkdownRoute
indexMarkdownRoute = MarkdownRoute $ "index" :| []

-- | Convert foo/bar.md to a @MarkdownRoute@
--
-- If the file is not a Markdown file, return Nothing.
mkMarkdownRoute :: FilePath -> Maybe MarkdownRoute
mkMarkdownRoute = \case
  (splitExtension -> (fp, ".md")) ->
    let slugs = fromString . toString . T.dropWhileEnd (== '/') . toText <$> splitPath fp
     in MarkdownRoute <$> nonEmpty slugs
  _ ->
    Nothing

markdownRouteSourcePath :: MarkdownRoute -> FilePath
markdownRouteSourcePath r =
  if r == indexMarkdownRoute
    then "index.md"
    else toString (T.intercalate "/" $ fmap Slug.unSlug $ toList $ unMarkdownRoute r) <> ".md"

-- | Filename of the markdown file without extension
markdownRouteFileBase :: MarkdownRoute -> Text
markdownRouteFileBase =
  Slug.unSlug . head . NE.reverse . unMarkdownRoute

-- | For use in breadcrumbs
markdownRouteInits :: MarkdownRoute -> NonEmpty MarkdownRoute
markdownRouteInits (MarkdownRoute ("index" :| [])) =
  one indexMarkdownRoute
markdownRouteInits (MarkdownRoute (slug :| rest')) =
  indexMarkdownRoute :| case nonEmpty rest' of
    Nothing ->
      one $ MarkdownRoute (one slug)
    Just rest ->
      MarkdownRoute (one slug) : go (one slug) rest
  where
    go :: NonEmpty Slug -> NonEmpty Slug -> [MarkdownRoute]
    go x (y :| ys') =
      let this = MarkdownRoute (x <> one y)
       in case nonEmpty ys' of
            Nothing ->
              one this
            Just ys ->
              this : go (unMarkdownRoute this) ys

-- ------------------------
-- Our site model
-- ------------------------

-- | This is our Ema "model" -- the app state used to generate our site.
--
-- It contains the list of all markdown files, parsed as Pandoc AST.
data Model = Model
  { modelDocs :: Map MarkdownRoute (Meta, Pandoc),
    modelNav :: [Tree Slug],
    -- | The ID is used to make the CSS url, so we are not caching stale
    -- Tailwind
    modelId :: UUID
  }
  deriving stock (Eq, Show)

emptyModel :: IO Model
emptyModel = Model mempty mempty <$> UUID.nextRandom

data Meta = Meta
  { -- | Indicates the order of the Markdown file in sidebar tree, relative to
    -- its siblings.
    order :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance Default Meta where
  def = Meta Nothing

modelLookup :: MarkdownRoute -> Model -> Maybe Pandoc
modelLookup k =
  fmap snd . modelLookup' k

modelLookupMeta :: MarkdownRoute -> Model -> Meta
modelLookupMeta k =
  maybe def fst . modelLookup' k

modelLookup' :: MarkdownRoute -> Model -> Maybe (Meta, Pandoc)
modelLookup' k =
  Map.lookup k . modelDocs

modelMember :: MarkdownRoute -> Model -> Bool
modelMember k =
  Map.member k . modelDocs

modelInsert :: MarkdownRoute -> (Meta, Pandoc) -> Model -> Model
modelInsert k v model =
  let modelDocs' = Map.insert k v (modelDocs model)
   in model
        { modelDocs = modelDocs',
          modelNav =
            PathTree.treeInsertPathMaintainingOrder
              (\k' -> order $ maybe def fst $ Map.lookup (MarkdownRoute k') modelDocs')
              (unMarkdownRoute k)
              (modelNav model)
        }

modelDelete :: MarkdownRoute -> Model -> Model
modelDelete k model =
  model
    { modelDocs = Map.delete k (modelDocs model),
      modelNav = PathTree.treeDeletePath (unMarkdownRoute k) (modelNav model)
    }

routeEncoder :: RouteEncoder MarkdownRoute Model
routeEncoder = unsafeMkRouteEncoder (const encodeRoute) (const decodeRoute) allRoutes
  where
    encodeRoute (MarkdownRoute slugs) =
      toString $ T.intercalate "/" (Slug.unSlug <$> toList slugs) <> ".html"
    decodeRoute fp =
      if null fp
        then pure indexMarkdownRoute
        else do
          basePath <- T.stripSuffix ".html" (toText fp)
          slugs <- nonEmpty $ fromString . toString <$> T.splitOn "/" basePath
          pure $ MarkdownRoute slugs
    -- Routes to write when generating the static site.
    allRoutes (Map.keys . modelDocs -> mdRoutes) =
      mdRoutes

-- ------------------------
-- Main entry point
-- ------------------------

log :: MonadLogger m => Text -> m ()
log = logInfoNS "ema-template"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "ema-template"

type NonEmptyLVar m a =
  ( -- Initial value
    a,
    -- Generator for subsequent values
    LVar a -> m ()
  )

markdownSite :: Site Model MarkdownRoute
markdownSite =
  Site
    { siteName = "ema-template",
      siteRender = render,
      siteModelRunner = const patch,
      siteRouteEncoder = routeEncoder
    }
  where
    patch :: (MonadIO m, MonadUnliftIO m, MonadLogger m) => m (NonEmptyLVar m Model)
    patch = do
      model0 <- liftIO emptyModel
      -- FIXME: initial model should be complete
      -- This is the place where we can load and continue to modify our "model".
      -- You will use `LVar.set` and `LVar.modify` to modify the model.
      --
      -- It is a run in a (long-running) thread of its own.
      --
      -- We use the FileSystem helper to directly "mount" our files on to the
      -- LVar.
      let pats = [((), "**/*.md")]
          ignorePats = [".*"]
      UnionMount.mountOnLVar "." pats ignorePats model0 $ \() fp action -> do
        case action of
          UnionMount.Refresh _ () -> do
            mData <- readSource fp
            pure $ maybe id (uncurry modelInsert) mData
          UnionMount.Delete ->
            pure $ maybe id modelDelete $ mkMarkdownRoute fp
    readSource :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe (MarkdownRoute, (Meta, Pandoc)))
    readSource fp =
      runMaybeT $ do
        r :: MarkdownRoute <- MaybeT $ pure $ mkMarkdownRoute fp
        logD $ "Reading " <> toText fp
        s <- readFileText fp
        pure
          ( r,
            either (throw . BadMarkdown) (first $ fromMaybe def) $
              Commonmark.parseMarkdownWithFrontMatter @Meta Commonmark.fullMarkdownSpec fp s
          )

newtype BadMarkdown = BadMarkdown Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- ------------------------
-- Our site HTML
-- ------------------------

render :: Some Ema.CLI.Action -> RouteEncoder MarkdownRoute Model -> Model -> MarkdownRoute -> Ema.Asset LByteString
render act enc model r =
  Ema.AssetGenerated Ema.Html $ renderHtml act enc model r

renderHtml :: Some Ema.CLI.Action -> RouteEncoder MarkdownRoute Model -> Model -> MarkdownRoute -> LByteString
renderHtml emaAction enc model r = do
  case modelLookup' r model of
    Nothing ->
      -- In dev server mode, Ema will display the exceptions in the browser.
      -- In static generation mode, they will cause the generation to crash.
      throw $ BadRoute r
    Just (meta, doc) -> do
      -- You can return your own HTML string here, but we use the Tailwind+Blaze helper
      layoutWith "en" "UTF-8" (headHtml emaAction model r doc) $
        bodyHtml enc model r meta doc
  where
    -- A general HTML layout
    layoutWith :: H.AttributeValue -> H.AttributeValue -> H.Html -> H.Html -> LByteString
    layoutWith lang encoding appHead appBody = RU.renderHtml $ do
      H.docType
      H.html ! A.lang lang $ do
        H.head $ do
          H.meta ! A.charset encoding
          -- This makes the site mobile friendly by default.
          H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
          appHead
        appBody

tailwindCssUrl :: (Semigroup a, IsString a) => Some Ema.CLI.Action -> Model -> a
tailwindCssUrl emaAction model =
  "static/tailwind.css"
    <> if Ema.CLI.isLiveServer emaAction
      then -- Force the browser to reload the CSS
        "?" <> show (modelId model)
      else ""

headHtml :: Some Ema.CLI.Action -> Model -> MarkdownRoute -> Pandoc -> H.Html
headHtml emaAction model r doc = do
  if Ema.CLI.isLiveServer emaAction
    then H.base ! A.href "/"
    else -- Since our URLs are all relative, and GitHub Pages uses a non-root base
    -- URL, we should specify it explicitly. Note that this is not necessary if
    -- you are using a CNAME.
      H.base ! A.href "https://srid.github.io/ema-template/"
  H.title $
    H.text $
      if r == indexMarkdownRoute
        then "Ema – next-gen Haskell static site generator"
        else lookupTitle doc r <> " – Ema"
  H.meta ! A.name "description" ! A.content "Ema static site generator (Jamstack) in Haskell"
  favIcon
  H.link ! A.rel "stylesheet" ! A.href (tailwindCssUrl emaAction model)
  -- Make this a PWA and w/ https://web.dev/themed-omnibox/
  H.link ! A.rel "manifest" ! A.href "manifest.json"
  H.meta ! A.name "theme-color" ! A.content "#DB2777"
  unless (r == indexMarkdownRoute) prismJs
  where
    prismJs = do
      H.unsafeByteString . encodeUtf8 $
        [text|
        <link href="https://cdn.jsdelivr.net/npm/prismjs@1.23.0/themes/prism-tomorrow.css" rel="stylesheet" />
        <script src="https://cdn.jsdelivr.net/combine/npm/prismjs@1.23.0/prism.min.js,npm/prismjs@1.23.0/plugins/autoloader/prism-autoloader.min.js"></script>
        |]
    favIcon = do
      H.unsafeByteString . encodeUtf8 $
        [text|
        <link href="static/logo.svg" rel="icon" />
        |]

data ContainerType
  = -- | The row representing title part of the site
    CHeader
  | -- | The row representing the main part of the site. Sidebar lives here, as well as <main>
    CBody
  deriving stock (Eq, Show)

containerLayout :: ContainerType -> H.Html -> H.Html -> H.Html
containerLayout ctype sidebar w = do
  H.div ! A.class_ "px-2 grid grid-cols-12" $ do
    let sidebarCls = case ctype of
          CHeader -> ""
          CBody -> "md:sticky md:top-0 md:h-screen overflow-x-auto"
    H.div ! A.class_ ("hidden md:mr-4 md:block md:col-span-3 " <> sidebarCls) $ do
      sidebar
    H.div ! A.class_ "col-span-12 md:col-span-9" $ do
      w

mdUrl :: RouteEncoder r model -> model -> r -> Text
mdUrl enc model r =
  Ema.routeUrl enc model r

bodyHtml :: RouteEncoder MarkdownRoute Model -> Model -> MarkdownRoute -> Meta -> Pandoc -> H.Html
bodyHtml enc model r meta doc = do
  H.div ! A.class_ "container mx-auto xl:max-w-screen-lg" $ do
    -- Header row
    let sidebarLogo =
          H.div ! A.class_ "mt-2 h-full flex pl-2 space-x-2 items-end" $ do
            H.a ! A.href (H.toValue $ mdUrl enc model indexMarkdownRoute) $
              H.img ! A.class_ "z-50 transition transform hover:scale-125 hover:opacity-80 h-20" ! A.src "static/logo.svg"
    containerLayout CHeader sidebarLogo $ do
      H.div ! A.class_ "flex justify-center items-center" $ do
        H.h1 ! A.class_ "text-6xl mt-2 mb-2 text-center pb-2" $ H.text $ lookupTitle doc r
    -- Main row
    containerLayout CBody (H.div ! A.class_ "bg-indigo-100 shadow-lg shadow-indigo-300/50 pt-1 pb-2" $ renderSidebarNav enc model r) $ do
      renderBreadcrumbs enc model r
      renderPandoc $
        doc
          & withoutH1 -- Eliminate H1, because we are rendering it separately (see above)
          & rewriteLinks
            -- Rewrite .md links to @MarkdownRoute@
            ( \url -> fromMaybe url $ do
                guard $ not $ "://" `T.isInfixOf` url
                target <- mkMarkdownRoute $ toString url
                -- Check that .md links are not broken
                if modelMember target model
                  then pure $ mdUrl enc model target
                  else throw $ BadRoute target
            )
      H.div ! A.class_ "text-xs text-gray-400 mt-4" $ do
        -- Just for debuggging
        H.toHtml $ Shower.shower meta
      H.footer ! A.class_ "flex justify-center items-center space-x-4 my-8 text-center text-gray-500" $ do
        let editUrl = fromString $ "https://github.com/srid/ema-template/edit/master/content/" <> markdownRouteSourcePath r
        H.a ! A.href editUrl ! A.title "Edit this page on GitHub" $ editIcon
        H.div $ do
          "Powered by "
          H.a ! A.class_ "font-bold" ! A.href "https://github.com/srid/ema" $ "Ema"
  where
    editIcon =
      H.unsafeByteString $
        encodeUtf8
          [text|
          <svg xmlns="http://www.w3.org/2000/svg" class="w-5 h-5" viewBox="0 0 20 20" fill="currentColor">
            <path d="M17.414 2.586a2 2 0 00-2.828 0L7 10.172V13h2.828l7.586-7.586a2 2 0 000-2.828z" />
            <path fill-rule="evenodd" d="M2 6a2 2 0 012-2h4a1 1 0 010 2H4v10h10v-4a1 1 0 112 0v4a2 2 0 01-2 2H4a2 2 0 01-2-2V6z" clip-rule="evenodd" />
          </svg>
          |]

renderSidebarNav :: RouteEncoder MarkdownRoute Model -> Model -> MarkdownRoute -> H.Html
renderSidebarNav enc model currentRoute = do
  -- Drop toplevel index.md from sidebar tree (because we are linking to it manually)
  let navTree = PathTree.treeDeleteChild "index" $ modelNav model
  go [] navTree
  where
    go parSlugs xs =
      H.div ! A.class_ "pl-2" $ do
        forM_ xs $ \(Node slug children) -> do
          let hereRoute = MarkdownRoute $ NE.reverse $ slug :| parSlugs
          renderRoute (if null parSlugs || not (null children) then "" else "text-gray-600") hereRoute
          go ([slug] <> parSlugs) children
    renderRoute c r = do
      let linkCls = if r == currentRoute then "text-yellow-600 font-bold" else ""
      H.div ! A.class_ ("my-2 " <> c) $ H.a ! A.class_ (" hover:text-black  " <> linkCls) ! A.href (H.toValue $ mdUrl enc model r) $ H.toHtml $ lookupTitleForgiving model r

renderBreadcrumbs :: RouteEncoder MarkdownRoute Model -> Model -> MarkdownRoute -> H.Html
renderBreadcrumbs enc model r = do
  whenNotNull (init $ markdownRouteInits r) $ \(toList -> crumbs) ->
    H.div ! A.class_ "w-full text-gray-600 mt-4 block md:hidden" $ do
      H.div ! A.class_ "flex justify-center" $ do
        H.div ! A.class_ "w-full bg-white py-2 rounded" $ do
          H.ul ! A.class_ "flex text-gray-500 text-sm lg:text-base" $ do
            forM_ crumbs $ \crumb ->
              H.li ! A.class_ "inline-flex items-center" $ do
                H.a ! A.class_ "px-1 font-bold bg-yellow-500 text-gray-50 rounded"
                  ! A.href (fromString . toString $ mdUrl enc model crumb)
                  $ H.text $ lookupTitleForgiving model crumb
                rightArrow
            H.li ! A.class_ "inline-flex items-center text-gray-600" $ do
              H.a $ H.text $ lookupTitleForgiving model r
  where
    rightArrow =
      H.unsafeByteString $
        encodeUtf8
          [text|
          <svg fill="currentColor" viewBox="0 0 20 20" class="w-auto h-5 text-gray-400"><path fill-rule="evenodd" d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z" clip-rule="evenodd"></path></svg>
          |]

-- | This accepts if "${folder}.md" doesn't exist, and returns "folder" as the
-- title.
lookupTitleForgiving :: Model -> MarkdownRoute -> Text
lookupTitleForgiving model r =
  fromMaybe (markdownRouteFileBase r) $ do
    doc <- modelLookup r model
    is <- getPandocH1 doc
    pure $ plainify is

lookupTitle :: Pandoc -> MarkdownRoute -> Text
lookupTitle doc r =
  maybe (Slug.unSlug $ last $ unMarkdownRoute r) plainify $ getPandocH1 doc

-- ------------------------
-- Pandoc transformer
-- ------------------------

rewriteLinks :: (Text -> Text) -> Pandoc -> Pandoc
rewriteLinks f =
  W.walk $ \case
    B.Link attr is (url, title) ->
      B.Link attr is (f url, title)
    x -> x

-- ------------------------
-- Pandoc renderer
-- ------------------------

renderPandoc :: Pandoc -> H.Html
renderPandoc doc = do
  -- "prose" is from https://tailwindcss.com/docs/typography-plugin
  let proseStyle =
        "prose-a:underline prose-a:decoration-indigo-700 prose-a:decoration-wavy prose-a:decoration-2 hover:prose-a:decoration-4"
  H.article ! A.class_ ("prose " <> proseStyle) $ do
    H.unsafeByteString . either (error . show) encodeUtf8 $
      Pandoc.runPure $ Pandoc.writeHtml5String writerSettings doc
  where
    writerSettings :: Pandoc.WriterOptions
    writerSettings = def {Pandoc.writerExtensions = exts}
    exts :: Pandoc.Extensions
    exts =
      mconcat
        [ Pandoc.extensionsFromList
            [ Pandoc.Ext_fenced_code_attributes,
              Pandoc.Ext_auto_identifiers,
              Pandoc.Ext_smart
            ],
          Pandoc.githubMarkdownExtensions
        ]

-- ------------------------
-- Pandoc AST helpers
-- ------------------------

getPandocH1 :: Pandoc -> Maybe [B.Inline]
getPandocH1 = listToMaybe . W.query go
  where
    go :: B.Block -> [[B.Inline]]
    go = \case
      B.Header 1 _ inlines ->
        [inlines]
      _ ->
        []

withoutH1 :: Pandoc -> Pandoc
withoutH1 (Pandoc meta (B.Header 1 _ _ : rest)) =
  Pandoc meta rest
withoutH1 doc =
  doc

-- | Convert Pandoc AST inlines to raw text.
plainify :: [B.Inline] -> Text
plainify = W.query $ \case
  B.Str x -> x
  B.Code _attr x -> x
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> " "
  B.RawInline _fmt s -> s
  B.Math _mathTyp s -> s
  -- Ignore the rest of AST nodes, as they are recursively defined in terms of
  -- `Inline` which `W.query` will traverse again.
  _ -> ""
