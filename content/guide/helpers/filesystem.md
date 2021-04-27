# Working with files

If your static site is generated depending on local files on disk, the general flow of things is as follows:

```haskell
runEma render $ \model -> do
  -- Load everything on launch
  initialModel <- loadFilesAndBuildModel
  LVar.set model initialModel
  -- Continue to monitor and update the model
  observeFileSystem $ \action -> 
    LVar.modify model $ applyAction action
```

For monitoring local files on disk you would typically use something like [fsnotify](https://hackage.haskell.org/package/fsnotify) in place of `observeFileSystem`. What is the point of doing this? To support [hot reload](concepts/hot-reload.md) on _data_ change. Imagine that your static site is generated based on Markdown files as well as HTML templates on disk. If either the Markdown file, or a HTML templates changes, we want the web browser to hot reload instantly. This is enabled by storing both in the [model](guide/model.md) and using [LVar](concepts/lvar.md) to update it over time.

For filesystem changes, Ema provides a helper based on `fsnotify` in the `Ema.Helper.FileSystem` module. You can use it as follows

```haskell
type Model = Map FilePath Text

Ema.runEma render $ \model -> do
  LVar.set model =<< do
    mdFiles <- FileSystem.filesMatching "." ["**/*.md"]
    forM mdFiles readMarkdown
      <&> Map.fromList 
  FileSystem.onChange "." $ \fp -> \case
    FileSystem.Update ->
      when (takeExtension fp == ".md") $ do
        log $ "Update: " <> fp 
        s <- readFileText fp
        LVar.modify model $ Map.insert fp s
    FileSystem.Delete ->
      whenJust (takeExtension fp == ".md") $ do
        log $ "Delete: " <> fp
        LVar.modify model $ Map.delete fp
```
