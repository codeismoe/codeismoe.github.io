{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

import Data.Bifunctor
import Data.Foldable (foldrM)
import Data.Functor
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import Hakyll
import qualified Network.Wai.Application.Static as Static
import Text.Pandoc
import Text.Pandoc.Highlighting (monochrome, styleToCss)
import qualified Text.Pandoc.UTF8 as T
import Text.Pandoc.Walk
import WaiAppStatic.Types (File (fileName), Piece (fromPiece), toPiece)

main :: IO ()
main = hakyllWith (defaultConfiguration {previewSettings = serverSettings, destinationDirectory = "docs/"}) $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["about.org", "contact.org"]) $ do
    route $ setExtension "html"
    compile $ do
      useMetaPandocCompiler
        >>= uncurry (loadAndApplyTemplate "templates/default.html") . first (<> postCtx)
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      useMetaPandocCompiler
        >>= uncurry (templates ["templates/post.html", "templates/default.html"]) . first (<> postCtx)
        >>= relativizeUrls

  create ["archive.html"] $ do
    route $ setExtension "html"
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ do
      makeItem $ styleToCss myPandocStyle

  create ["CNAME"] $ do
    route idRoute
    compile $ makeItem ("lily.bike" :: String)

  match "index.html" $ do
    route $ idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

serverSettings :: FilePath -> Static.StaticSettings
serverSettings path =
  baseSettings
    { Static.ssGetMimeType = getMimeType,
      -- github wants .html in the files but not in the routes, adjust for this in dev
      Static.ssLookupFile = Static.ssLookupFile baseSettings . addHtml
    }
  where
    addHtml [] = [] -- ensure index works still lmao
    addHtml p =
      let file = fromPiece $ last p
       in maybe p (\x -> init p <> [x]) (toPiece (if '.' `T.elem` file then file else file <> ".html"))
    baseSettings = Static.defaultFileServerSettings path
    defaultGetMimeType = Static.ssGetMimeType baseSettings

    -- Overrides MIME type for files with no extension
    -- so that HTML pages need no extension.
    getMimeType file =
      if T.elem '.' (fromPiece $ fileName file)
        then defaultGetMimeType file
        else return "text/html"

fromPandocMeta :: Meta -> Context String
fromPandocMeta (Meta m) = Context $ \k _ _ ->
  case M.lookup (T.pack k) m of
    Just (MetaInlines s) ->
      return $
        StringField $
          T.unpack $
            T.concat $
              map
                ( \case
                    Str s -> s
                    Space -> " "
                    _notUsed -> ""
                )
                s
    Just s -> fail $ "'" ++ k ++ "' is not a string?"
    Nothing -> fail $ "No '" ++ k ++ "' found in Pandoc Metadata"

postCtx :: Context String
postCtx =
  field "url" clean
    <> dateField "date" "%B %e, %Y"
      `mappend` defaultContext
  where
    clean item = do
      path <- getRoute (itemIdentifier item)
      case path of
        Nothing -> noResult "no route for identifier"
        Just s -> pure . cleanupIndexUrl . toUrl $ s

cleanupIndexUrl :: String -> String
cleanupIndexUrl url@('/' : _) -- only clean up local URLs
  | Nothing <- prefix = url -- does not end with index.html
  | Just s <- prefix = s -- clean up index.html from URL
  where
    prefix = needlePrefix "index.html" url
cleanupIndexUrl url = url

useMetaPandocCompiler :: Compiler (Context String, Item String)
useMetaPandocCompiler = do
  body <- getResourceBody
  i@(Item {itemBody = Pandoc m _}) <- readPandoc body
  return (fromPandocMeta m, writePandocWith (defaultHakyllWriterOptions {writerHighlightStyle = Just myPandocStyle}) i)

myPandocStyle = monochrome

templates :: [Identifier] -> Context String -> Item String -> Compiler (Item String)
templates tpls ctx item = foldrM (\tpl comp -> loadAndApplyTemplate tpl ctx comp) item tpls
