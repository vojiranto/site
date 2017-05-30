--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Control.Monad
import           Text.Pandoc
import           Hakyll
import           Data.Text.Lazy.IO (writeFile)
import           Data.Time
import           Clay
import           Prelude hiding (div, writeFile)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- собираем css
--  writeFile "css/default.css" defaultCss
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")    
    let baseCtx = tagCloudField "tagcloud" 80.0 100.0 tags <> defaultContext
        postCtx' = postCtx baseCtx 

    match (fromList ["about.md", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" baseCtx
            >>= relativizeUrls
        -- build up tags

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags baseCtx)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags baseCtx)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx' (return posts) `mappend`
                    constField "title" "Архив"               `mappend`
                    baseCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Заметки с тегом «" ++ tag ++ "»"
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx' (return posts)
                      `mappend` baseCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx' (return posts) `mappend`
                    constField "title" "Главная"              `mappend`
                    baseCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx baseCtx =
    dateFieldWith rusTimeLocale "date" "%e-го %B %Y-го года." `mappend`
    baseCtx

postCtxWithTags tags baseCtx = tagsField "tags" tags `mappend` postCtx baseCtx

rusTimeLocale :: TimeLocale
rusTimeLocale =  defaultTimeLocale {
        months = [("января",   "Jan"), ("февраля",  "Feb"),
                  ("марта",    "Mar"), ("апреля",   "Apr"),
                  ("мая",      "May"), ("июня",     "Jun"),
                  ("июля",     "Jul"), ("августа",  "Aug"),
                  ("сентября", "Sep"), ("октября",  "Oct"),
                  ("ноября",   "Nov"), ("декабря",  "Dec")]}

