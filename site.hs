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
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    let baseCtx = tagCloudField "tagcloud" 80.0 100.0 tags <> defaultContext
        postCtx' = postCtx baseCtx

    match (fromList ["about.md", "contact.markdown", "links.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" baseCtx
            >>= relativizeUrls
        -- build up tags

    -- строим подмножество сайта с текстами.
    textTags <- buildTags "texts/*" (fromCapture "text_tags/*.html")
    
    let textBaseCtx = tagCloudField "tagcloud" 80.0 100.0 textTags <> defaultContext
        textPostCtx' = postCtx textBaseCtx

    match "texts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags textTags textBaseCtx)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags textTags textBaseCtx)
            >>= relativizeUrls

    match "eng-text/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags textTags textBaseCtx)
            >>= loadAndApplyTemplate "templates/default-eng.html" (postCtxWithTags textTags textBaseCtx)
            >>= relativizeUrls


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
    
    create ["text_archive.html"] $ do
        route idRoute
        compile $ do
            posts<- recentFirst =<< loadAll "texts/*"
            let archiveCtx =
                    listField "posts" textPostCtx' (return posts) `mappend`
                    constField "title" "Архив текстов"            `mappend`
                    textBaseCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/text_archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
    

    -- строим систему списков для тегов.
    tagsRules textTags $ \tag pattern -> do
        let title = "Тексты с тегом «" ++ tag ++ "»"
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" textPostCtx' (return posts)
                      `mappend` textBaseCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
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
            -- FIXME    Здесь нужно ограничить количество загружаемых постов.
            --          это можно сделать заменив функцию recentFirst на свою
            --          стандартное определение смотри ниже.
            posts <- liftM (take 5) . recentFirst =<< loadAll "posts/*"
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

{-
-- | The reverse of 'chronological'
recentFirst :: MonadMetadata m => [Item a] -> m [Item a]
recentFirst = liftM reverse . chronological

-- | Sort pages chronologically. Uses the same method as 'dateField' for
-- extracting the date.
chronological :: MonadMetadata m => [Item a] -> m [Item a]
chronological =
    sortByM $ getItemUTC defaultTimeLocale . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

https://www.stackage.org/haddock/lts-8.15/hakyll-4.9.5.1/src/Hakyll.Web.Template.List.html#recentFirst
-}
