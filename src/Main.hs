{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | This code generates a site based on Markdown files, rendering them using Pandoc.
-- As such it might be a little too involved. Simpler examples can be found here,
--   https://github.com/srid/ema/tree/master/src/Ema/Example
module Main where

import Control.Exception (throw)
import Control.Monad.Logger
import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Tree (Tree (Node))
import Ema (Ema (..), Slug)
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Markdown as Markdown
import qualified Ema.Helper.PathTree as PathTree
import qualified Ema.Helper.Tailwind as Tailwind
import NeatInterpolation (text)
import System.FilePath (splitExtension, splitPath)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Pandoc.Walk as W

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
  deriving (Eq, Ord, Show)

newtype BadRoute = BadRoute MarkdownRoute
  deriving (Show, Exception)

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
    else toString (T.intercalate "/" $ fmap Ema.unSlug $ toList $ unMarkdownRoute r) <> ".md"

-- | Filename of the markdown file without extension
markdownRouteFileBase :: MarkdownRoute -> Text
markdownRouteFileBase =
  Ema.unSlug . head . NE.reverse . unMarkdownRoute

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
    modelNav :: [Tree Slug]
  }
  deriving (Eq, Show)

instance Default Model where
  def = Model mempty mempty

data Meta = Meta
  { -- | Indicates the order of the Markdown file in sidebar tree, relative to
    -- its siblings.
    order :: Maybe Int
  }
  deriving (Eq, Show, Generic, FromJSON)

instance Default Meta where
  def = Meta Nothing

modelLookup :: MarkdownRoute -> Model -> Maybe Pandoc
modelLookup k =
  fmap snd . Map.lookup k . modelDocs

modelLookupMeta :: MarkdownRoute -> Model -> Meta
modelLookupMeta k =
  maybe def fst . Map.lookup k . modelDocs

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

-- | Once we have a "model" and "route" (as defined above), we should define the
-- @Ema@ typeclass to tell Ema how to decode/encode our routes, as well as the
-- list of routes to generate the static site with.
--
-- We use `Either` to represent either a static file route or a Markdown
-- generated route.
instance Ema Model (Either FilePath MarkdownRoute) where
  encodeRoute _model = \case
    Left fp -> fp
    Right (MarkdownRoute slugs) ->
      toString $ T.intercalate "/" (Ema.unSlug <$> toList slugs) <> ".html"

  decodeRoute _model fp = do
    if "static/" `T.isPrefixOf` toText fp
      then pure $ Left fp
      else do
        if null fp
          then pure $ Right indexMarkdownRoute
          else do
            basePath <- T.stripSuffix ".html" (toText fp)
            slugs <- nonEmpty $ fromString . toString <$> T.splitOn "/" basePath
            pure $ Right $ MarkdownRoute slugs

  -- Routes to write when generating the static site.
  allRoutes (Map.keys . modelDocs -> mdRoutes) =
    [Left "static"]
      <> fmap Right mdRoutes

-- ------------------------
-- Main entry point
-- ------------------------

log :: MonadLogger m => Text -> m ()
log = logInfoNS "ema-template"

logD :: MonadLogger m => Text -> m ()
logD = logDebugNS "ema-template"

main :: IO ()
main =
  -- runEma handles the CLI and starts the dev server (or generate static site
  -- if `gen` argument is passed).  It is designed to work well with ghcid
  -- (which is what the bin/run script uses).
  Ema.runEma render $ \_act model -> do
    -- This is the place where we can load and continue to modify our "model".
    -- You will use `LVar.set` and `LVar.modify` to modify the model.
    --
    -- It is a run in a (long-running) thread of its own.
    --
    -- We use the FileSystem helper to directly "mount" our files on to the
    -- LVar.
    let pats = [((), "**/*.md")]
        ignorePats = [".*"]
    void . FileSystem.mountOnLVar "." pats ignorePats model def $ \() fp action -> do
      case action of
        FileSystem.Refresh _ () -> do
          mData <- readSource fp
          pure $ maybe id (uncurry modelInsert) mData
        FileSystem.Delete ->
          pure $ maybe id modelDelete $ mkMarkdownRoute fp
  where
    readSource :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe (MarkdownRoute, (Meta, Pandoc)))
    readSource fp =
      runMaybeT $ do
        r :: MarkdownRoute <- MaybeT $ pure $ mkMarkdownRoute fp
        logD $ "Reading " <> toText fp
        s <- readFileText fp
        pure
          ( r,
            either (throw . BadMarkdown) (first $ fromMaybe def) $
              Markdown.parseMarkdownWithFrontMatter @Meta Markdown.fullMarkdownSpec fp s
          )

newtype BadMarkdown = BadMarkdown Text
  deriving (Show, Exception)

-- ------------------------
-- Our site HTML
-- ------------------------

render :: Ema.CLI.Action -> Model -> Either FilePath MarkdownRoute -> Ema.Asset LByteString
render act model = \case
  Left fp ->
    -- This instructs ema to treat this route "as is" (ie. a static file; no generation)
    -- The argument `fp` refers to the absolute path to the static file.
    Ema.AssetStatic fp
  Right r ->
    -- Generate a Html route; hot-reload is enabled.
    Ema.AssetGenerated Ema.Html $ renderHtml act model r

renderHtml :: Ema.CLI.Action -> Model -> MarkdownRoute -> LByteString
renderHtml emaAction model r = do
  case modelLookup r model of
    Nothing ->
      -- In dev server mode, Ema will display the exceptions in the browser.
      -- In static generation mode, they will cause the generation to crash.
      throw $ BadRoute r
    Just doc -> do
      -- You can return your own HTML string here, but we use the Tailwind+Blaze helper
      Tailwind.layout emaAction (headHtml emaAction r doc) (bodyHtml model r doc)

headHtml :: Ema.CLI.Action -> MarkdownRoute -> Pandoc -> H.Html
headHtml emaAction r doc = do
  case emaAction of
    Ema.CLI.Generate _ ->
      -- Since our URLs are all relative, and GitHub Pages uses a non-root base
      -- URL, we should specify it explicitly. Note that this is not necessary if
      -- you are using a CNAME.
      H.base ! A.href "https://srid.github.io/ema-template/"
    _ ->
      H.base ! A.href "/"
  H.title $
    H.text $
      if r == indexMarkdownRoute
        then "Ema – next-gen Haskell static site generator"
        else lookupTitle doc r <> " – Ema"
  H.meta ! A.name "description" ! A.content "Ema static site generator (Jamstack) in Haskell"
  favIcon
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
  deriving (Eq, Show)

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

mdUrl :: Ema model (Either FilePath r) => model -> r -> Text
mdUrl model r =
  Ema.routeUrl model $ Right @FilePath r

bodyHtml :: Model -> MarkdownRoute -> Pandoc -> H.Html
bodyHtml model r doc = do
  H.div ! A.class_ "container mx-auto xl:max-w-screen-lg" $ do
    -- Header row
    let sidebarLogo =
          H.div ! A.class_ "mt-2 h-full flex pl-2 space-x-2 items-end" $ do
            H.a ! A.href (H.toValue $ mdUrl model indexMarkdownRoute) $
              H.img ! A.class_ "z-50 transition transform hover:scale-125 hover:opacity-80 h-20" ! A.src "static/logo.svg"
    containerLayout CHeader sidebarLogo $ do
      H.div ! A.class_ "flex justify-center items-center" $ do
        H.h1 ! A.class_ "text-6xl mt-2 mb-2 text-center pb-2" $ H.text $ lookupTitle doc r
    -- Main row
    containerLayout CBody (H.div ! A.class_ "bg-yellow-50 rounded pt-1 pb-2" $ renderSidebarNav model r) $ do
      renderBreadcrumbs model r
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
                  then pure $ mdUrl model target
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

renderSidebarNav :: Model -> MarkdownRoute -> H.Html
renderSidebarNav model currentRoute = do
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
      H.div ! A.class_ ("my-2 " <> c) $ H.a ! A.class_ (" hover:text-black  " <> linkCls) ! A.href (H.toValue $ mdUrl model r) $ H.toHtml $ lookupTitleForgiving model r

renderBreadcrumbs :: Model -> MarkdownRoute -> H.Html
renderBreadcrumbs model r = do
  whenNotNull (init $ markdownRouteInits r) $ \(toList -> crumbs) ->
    H.div ! A.class_ "w-full text-gray-600 mt-4 block md:hidden" $ do
      H.div ! A.class_ "flex justify-center" $ do
        H.div ! A.class_ "w-full bg-white py-2 rounded" $ do
          H.ul ! A.class_ "flex text-gray-500 text-sm lg:text-base" $ do
            forM_ crumbs $ \crumb ->
              H.li ! A.class_ "inline-flex items-center" $ do
                H.a ! A.class_ "px-1 font-bold bg-yellow-500 text-gray-50 rounded"
                  ! A.href (fromString . toString $ mdUrl model crumb)
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
    pure $ Markdown.plainify is

lookupTitle :: Pandoc -> MarkdownRoute -> Text
lookupTitle doc r =
  maybe (Ema.unSlug $ last $ unMarkdownRoute r) Markdown.plainify $ getPandocH1 doc

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
--
-- Note that we hardcode tailwind classes, because pandoc AST is not flexible
-- enough to provide attrs for all inlines/blocks. So we can't rely on Walk to
-- transform it.

renderPandoc :: Pandoc -> H.Html
renderPandoc (Pandoc _meta blocks) =
  mapM_ rpBlock blocks

rpBlock :: B.Block -> H.Html
rpBlock = \case
  B.Plain is ->
    mapM_ rpInline is
  B.Para is ->
    H.p ! A.class_ "my-2" $ mapM_ rpInline is
  B.LineBlock iss ->
    forM_ iss $ \is ->
      mapM_ rpInline is >> "\n"
  B.CodeBlock (id', classes, attrs) s ->
    -- Prism friendly classes
    let classes' = flip concatMap classes $ \cls -> [cls, "language-" <> cls]
     in H.div ! A.class_ "py-0.5 text-sm" $ H.pre ! rpAttr (id', classes', attrs) $ H.code ! rpAttr ("", classes', []) $ H.text s
  B.RawBlock (B.Format fmt) html ->
    if fmt == "html"
      then H.unsafeByteString $ encodeUtf8 html
      else throw Unsupported
  B.BlockQuote bs ->
    H.blockquote $ mapM_ rpBlock bs
  B.OrderedList _ bss ->
    H.ol ! A.class_ (listStyle <> " list-decimal") $
      forM_ bss $ \bs ->
        H.li ! A.class_ listItemStyle $ mapM_ rpBlock bs
  B.BulletList bss ->
    H.ul ! A.class_ (listStyle <> " list-disc") $
      forM_ bss $ \bs ->
        H.li ! A.class_ listItemStyle $ mapM_ rpBlock bs
  B.DefinitionList defs ->
    H.dl $
      forM_ defs $ \(term, descList) -> do
        mapM_ rpInline term
        forM_ descList $ \desc ->
          H.dd $ mapM_ rpBlock desc
  B.Header level attr is ->
    headerElem level ! rpAttr attr $ mapM_ rpInline is
  B.HorizontalRule ->
    H.hr
  B.Table {} ->
    throw Unsupported
  B.Div attr bs ->
    H.div ! rpAttr attr $ mapM_ rpBlock bs
  B.Null ->
    pure ()
  where
    listStyle = "list-inside ml-2 space-y-1 "
    listItemStyle = ""

headerElem :: Int -> H.Html -> H.Html
headerElem = \case
  1 -> H.h1 ! A.class_ "text-6xl mt-2 mb-2 text-center pb-2"
  2 -> H.h2 ! A.class_ ("text-5xl " <> my)
  3 -> H.h3 ! A.class_ ("text-4xl " <> my)
  4 -> H.h4 ! A.class_ ("text-3xl " <> my)
  5 -> H.h5 ! A.class_ ("text-2xl " <> my)
  6 -> H.h6 ! A.class_ ("text-xl " <> my)
  _ -> error "Invalid pandoc header level"
  where
    my = "mt-4 mb-2 text-gray-700"

rpInline :: B.Inline -> H.Html
rpInline = \case
  B.Str s -> H.toHtml s
  B.Emph is ->
    H.em $ mapM_ rpInline is
  B.Strong is ->
    H.strong $ mapM_ rpInline is
  B.Underline is ->
    H.u $ mapM_ rpInline is
  B.Strikeout is ->
    H.del $ mapM_ rpInline is
  B.Superscript is ->
    H.sup $ mapM_ rpInline is
  B.Subscript is ->
    H.sub $ mapM_ rpInline is
  B.Quoted qt is ->
    flip inQuotes qt $ mapM_ rpInline is
  B.Code attr s ->
    H.code ! rpAttr attr $ H.toHtml s
  B.Space -> " "
  B.SoftBreak -> " "
  B.LineBreak -> H.br
  B.RawInline _fmt s ->
    H.pre $ H.toHtml s
  B.Math _ _ ->
    throw Unsupported
  B.Link attr is (url, title) -> do
    let (cls, target) =
          if "://" `T.isInfixOf` url
            then ("text-yellow-600 hover:underline", targetBlank)
            else ("text-yellow-600 font-bold hover:bg-pink-50", mempty)
    H.a
      ! A.class_ cls
      ! A.href (H.textValue url)
      ! A.title (H.textValue title)
      ! target
      ! rpAttr attr
      $ mapM_ rpInline is
  B.Image attr is (url, title) ->
    H.img ! A.src (H.textValue url) ! A.title (H.textValue title) ! A.alt (H.textValue $ Markdown.plainify is) ! rpAttr attr
  B.Note _ ->
    throw Unsupported
  B.Span attr is ->
    H.span ! rpAttr attr $ mapM_ rpInline is
  x ->
    H.pre $ H.toHtml $ show @Text x
  where
    inQuotes :: H.Html -> B.QuoteType -> H.Html
    inQuotes w = \case
      B.SingleQuote -> "‘" >> w <* "’"
      B.DoubleQuote -> "“" >> w <* "”"

targetBlank :: H.Attribute
targetBlank =
  A.target "_blank" <> A.rel "noopener"

rpAttr :: B.Attr -> H.Attribute
rpAttr (id', classes, attrs) =
  let cls = T.intercalate " " classes
   in unlessNull id' (A.id (fromString . toString $ id'))
        <> unlessNull cls (A.class_ (fromString . toString $ cls))
        <> mconcat (fmap (\(k, v) -> H.dataAttribute (fromString . toString $ k) (fromString . toString $ v)) attrs)
  where
    unlessNull x f =
      if T.null x then mempty else f

data Unsupported = Unsupported
  deriving (Show, Exception)

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
