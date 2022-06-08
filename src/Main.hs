{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Commonmark.Simple qualified as Commonmark
import Control.Exception (throw)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, logDebugNS)
import Data.Default (Default (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Some (Some)
import Data.Text qualified as T
import Data.Tree (Tree (Node))
import Data.Tree.Path qualified as PathTree
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import NeatInterpolation (text)
import Network.URI.Slug (Slug)
import Network.URI.Slug qualified as Slug
import Optics.Core (Iso', iso, prism', (%))
import System.FilePath (splitExtension, splitPath, (</>))
import System.UnionMount qualified as UnionMount
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Walk qualified as W

import Ema
import Ema.CLI qualified
import Ema.Route.Encoder (RouteEncoder, htmlSuffixPrism, mapRouteEncoderModel, mapRouteEncoderRoute, mergeRouteEncoder, mkRouteEncoder)

main :: IO ()
main =
  void $ Ema.runSite @Route "./content"

-- ------------------------
-- Our site route
-- ------------------------

data Route
  = Route_Markdown MarkdownRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord)

-- | Represents the relative path to a source (.md) file under some directory.
newtype MarkdownRoute = MarkdownRoute {unMarkdownRoute :: NonEmpty Slug}
  deriving stock (Eq, Ord, Show)

newtype BadRoute = BadRoute MarkdownRoute
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Represents the top-level index.md
indexMarkdownRoute :: MarkdownRoute
indexMarkdownRoute = MarkdownRoute $ "index" :| []

{- | Convert foo/bar.md to a @MarkdownRoute@

 If the file is not a Markdown file, return Nothing.
-}
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

{- | This is our Ema "model" -- the app state used to generate our site.

  If your asset generating render function requires anything at all, it should
  go in here.
-}
data Model = Model
  { -- | Directory under which all content is kept.
    modelBaseDir :: FilePath
  , -- | Pandoc AST of all markdown files.
    modelDocs :: Map MarkdownRoute Pandoc
  , -- | Other files (to be served/ copied as-is)
    modelFiles :: Set FilePath
  , -- | Sidebar tree
    modelNav :: [Tree Slug]
  , -- | The ID is used to make the CSS url, so we are not caching stale
    -- Tailwind.  TODO: This should use md5 sum or num increment?
    modelId :: UUID
  , -- | Ema's CLI arguments stored in model, to later check if we are in live
    -- server.
    modelCliAction :: Some Ema.CLI.Action
  }
  deriving stock (Eq, Show)

emptyModel :: FilePath -> Some Ema.CLI.Action -> IO Model
emptyModel baseDir act = Model baseDir mempty mempty mempty <$> UUID.nextRandom <*> pure act

modelLookup :: MarkdownRoute -> Model -> Maybe Pandoc
modelLookup k =
  Map.lookup k . modelDocs

modelMember :: MarkdownRoute -> Model -> Bool
modelMember k =
  Map.member k . modelDocs

modelInsert :: MarkdownRoute -> Pandoc -> Model -> Model
modelInsert k v model =
  let modelDocs' = Map.insert k v (modelDocs model)
   in model
        { modelDocs = modelDocs'
        , modelNav =
            PathTree.treeInsertPath
              (unMarkdownRoute k)
              (modelNav model)
        }

modelInsertStaticFile :: FilePath -> Model -> Model
modelInsertStaticFile fp model = model {modelFiles = Set.insert fp (modelFiles model)}

modelDeleteStaticFile :: FilePath -> Model -> Model
modelDeleteStaticFile fp model = model {modelFiles = Set.delete fp (modelFiles model)}

modelDelete :: MarkdownRoute -> Model -> Model
modelDelete k model =
  model
    { modelDocs = Map.delete k (modelDocs model)
    , modelNav = PathTree.treeDeletePath (unMarkdownRoute k) (modelNav model)
    }

-- ------------------------
-- Route encoder
-- ------------------------

newtype StaticRoute = StaticRoute {unStaticRoute :: FilePath}
  deriving newtype (Eq, Ord, Show)

-- A route encoder is simply a Prism with (some) model as its context.
-- `mkRouteEncoder` enables you to create route encoders "manually" this way.
instance IsRoute StaticRoute where
  type RouteModel StaticRoute = Set FilePath
  routeEncoder = mkRouteEncoder $ \files ->
    let enc =
          unStaticRoute
        dec fp =
          StaticRoute fp <$ guard (Set.member fp files)
     in prism' enc dec
  allRoutes files =
    StaticRoute <$> toList files

instance IsRoute MarkdownRoute where
  type RouteModel MarkdownRoute = Set MarkdownRoute
  routeEncoder = mkRouteEncoder $ \mdRoutes ->
    let enc (MarkdownRoute slugs) =
          toString $ T.intercalate "/" (Slug.unSlug <$> toList slugs)
        dec fp = do
          guard $ not $ null fp
          slugs <- nonEmpty $ fromString . toString <$> T.splitOn "/" (toText fp)
          let r = MarkdownRoute slugs
          guard $ Set.member r mdRoutes
          pure r
     in htmlSuffixPrism % prism' enc dec
  allRoutes =
    toList

-- Route encoders can also be composed. Here, we use `mergeRouteEncoder` to
-- combine two route encoders.
instance IsRoute Route where
  type RouteModel Route = Model
  routeEncoder =
    -- TODO: Upstream this pattern?
    mergeRouteEncoder (routeEncoder @MarkdownRoute) (routeEncoder @StaticRoute)
      & mapRouteEncoderRoute routeDecomposition
      & mapRouteEncoderModel decomposeModel
  allRoutes model =
    fmap Route_Markdown (allRoutes @MarkdownRoute $ fst . decomposeModel $ model)
      <> fmap Route_Static (allRoutes @StaticRoute $ snd . decomposeModel $ model)

decomposeModel :: Model -> (Set MarkdownRoute, Set FilePath)
decomposeModel = Map.keysSet . modelDocs &&& modelFiles

routeDecomposition :: Iso' (Either MarkdownRoute StaticRoute) Route
routeDecomposition =
  iso (either Route_Markdown Route_Static) $ \case
    Route_Markdown r -> Left r
    Route_Static r -> Right r

-- ------------------------
-- Main entry point
-- ------------------------

instance EmaSite Route where
  type SiteArg Route = FilePath -- Content directory
  siteInput cliAct _ contentDir = do
    model0 <- liftIO $ emptyModel contentDir cliAct
    -- This function should return an Ema `Dynamic`, which is merely a tuple of
    -- the initial model value, and an updating function. The `unionmount`
    -- library returns this tuple, which we use to create the `Dynamic`.
    let pats = [(Md, "**/*.md"), (StaticFile, "**/*")]
        ignorePats = [".*"]
    Dynamic
      <$> UnionMount.mount contentDir pats ignorePats model0 handleUpdate
    where
      -- Take the file that got changed and update our in-memory `Model` accordingly.
      handleUpdate :: (MonadIO m, MonadLogger m, MonadLoggerIO m) => FileType -> FilePath -> UnionMount.FileAction () -> m (Model -> Model)
      handleUpdate = \case
        Md -> \fp -> \case
          UnionMount.Refresh _ _ -> do
            mData <- readSource fp
            pure $ maybe id (uncurry modelInsert) mData
          UnionMount.Delete ->
            pure $ maybe id modelDelete $ mkMarkdownRoute fp
        StaticFile -> \fp -> \case
          UnionMount.Refresh _ _ -> do
            pure $ modelInsertStaticFile fp
          UnionMount.Delete -> do
            pure $ modelDeleteStaticFile fp
      readSource :: (MonadIO m, MonadLogger m, MonadLoggerIO m) => FilePath -> m (Maybe (MarkdownRoute, Pandoc))
      readSource fp = runMaybeT $ do
        r :: MarkdownRoute <- MaybeT $ pure $ mkMarkdownRoute fp
        logD $ "Reading " <> toText fp
        s <- readFileText $ contentDir </> fp
        case Commonmark.parseMarkdownWithFrontMatter @(Map Text Text) Commonmark.fullMarkdownSpec fp s of
          Left err -> Ema.CLI.crash "ema-template" err
          Right (_mMeta, doc) -> pure (r, doc)
  siteOutput enc model = \case
    Route_Markdown r ->
      Ema.AssetGenerated Ema.Html $ renderHtml enc model r
    Route_Static (StaticRoute path) ->
      Ema.AssetStatic $ modelBaseDir model </> path

data FileType = Md | StaticFile deriving stock (Show, Eq, Ord)

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "ema-template"

-- ------------------------
-- Our site HTML
-- ------------------------

renderHtml :: RouteEncoder Model Route -> Model -> MarkdownRoute -> LByteString
renderHtml enc model r = do
  case modelLookup r model of
    Nothing ->
      -- In dev server mode, Ema will display the exceptions in the browser.
      -- In static generation mode, they will cause the generation to crash.
      throw $ BadRoute r
    Just doc -> do
      -- You can return your own HTML string here, but we use the Tailwind+Blaze helper
      layoutWith "en" "UTF-8" (headHtml model r doc) $
        bodyHtml enc model r doc
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

data NoTailwind = NoTailwind
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

tailwindCssUrl :: (Semigroup a, IsString a) => Model -> a
tailwindCssUrl model =
  if Set.member "tailwind.css" (modelFiles model)
    then forceReload "tailwind.css"
    else throw NoTailwind
  where
    -- Force the browser to reload the CSS
    forceReload url =
      url
        <> if Ema.CLI.isLiveServer (modelCliAction model)
          then "?" <> show (modelId model)
          else -- TODO: Need a way to invalidate browser cache for statically generated site
            ""

headHtml :: Model -> MarkdownRoute -> Pandoc -> H.Html
headHtml model r doc = do
  if Ema.CLI.isLiveServer (modelCliAction model)
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
  H.link ! A.href "logo.svg" ! A.rel "icon"
  H.link ! A.rel "stylesheet" ! A.href (tailwindCssUrl model)

containerLayout :: H.AttributeValue -> H.Html -> H.Html -> H.Html
containerLayout sidebarCls sidebar w = do
  H.div ! A.class_ "px-2 grid grid-cols-12" $ do
    H.div ! A.class_ ("hidden md:mr-4 md:block md:col-span-3 " <> sidebarCls) $ do
      sidebar
    H.div ! A.class_ "col-span-12 md:col-span-9" $ do
      w

bodyHtml :: RouteEncoder Model Route -> Model -> MarkdownRoute -> Pandoc -> H.Html
bodyHtml enc model r doc = do
  H.div ! A.class_ "container mx-auto xl:max-w-screen-lg" $ do
    -- Header row
    let sidebarLogo =
          H.div ! A.class_ "mt-2 h-full flex pl-2 space-x-2 items-end" $ do
            H.a ! A.href (H.toValue $ Ema.routeUrl enc model $ Route_Markdown indexMarkdownRoute) $
              H.img ! A.class_ "z-50 transition transform hover:scale-125 hover:opacity-80 h-20" ! A.src "logo.svg"
    containerLayout "" sidebarLogo $ do
      H.div ! A.class_ "flex justify-center items-center" $ do
        H.h1 ! A.class_ "text-6xl mt-2 mb-2 text-center pb-2" $ H.text $ lookupTitle doc r
    -- Main row
    containerLayout "md:sticky md:top-0 md:h-screen overflow-x-auto" (H.div ! A.class_ "bg-indigo-100 shadow-lg shadow-indigo-300/50 pt-1 pb-2" $ renderSidebarNav enc model r) $ do
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
                  then pure $ Ema.routeUrl enc model $ Route_Markdown target
                  else throw $ BadRoute target
            )
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

renderSidebarNav :: RouteEncoder Model Route -> Model -> MarkdownRoute -> H.Html
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
      H.div ! A.class_ ("my-2 " <> c) $ do
        H.a
          ! A.class_ (" hover:text-black  " <> linkCls)
          ! A.href (H.toValue $ Ema.routeUrl enc model $ Route_Markdown r)
          $ H.toHtml $ lookupTitleForgiving model r

renderBreadcrumbs :: RouteEncoder Model Route -> Model -> MarkdownRoute -> H.Html
renderBreadcrumbs enc model r = do
  whenNotNull (init $ markdownRouteInits r) $ \(toList -> crumbs) ->
    H.div ! A.class_ "w-full text-gray-600 mt-4 block md:hidden" $ do
      H.div ! A.class_ "flex justify-center" $ do
        H.div ! A.class_ "w-full bg-white py-2 rounded" $ do
          H.ul ! A.class_ "flex text-gray-500 text-sm lg:text-base" $ do
            forM_ crumbs $ \crumb ->
              H.li ! A.class_ "inline-flex items-center" $ do
                H.a ! A.class_ "px-1 font-bold bg-yellow-500 text-gray-50 rounded"
                  ! A.href (fromString . toString $ Ema.routeUrl enc model $ Route_Markdown crumb)
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

{- | This accepts if "${folder}.md" doesn't exist, and returns "folder" as the
 title.
-}
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
rewriteLinks f = W.walk $ \case
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
            [ Pandoc.Ext_fenced_code_attributes
            , Pandoc.Ext_auto_identifiers
            , Pandoc.Ext_smart
            ]
        , Pandoc.githubMarkdownExtensions
        ]

-- ------------------------
-- Pandoc AST helpers
-- ------------------------

getPandocH1 :: Pandoc -> Maybe [B.Inline]
getPandocH1 = listToMaybe . W.query go
  where
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
