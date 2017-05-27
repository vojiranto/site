--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import           Data.Time

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")    
    let baseCtx = tagCloudField "tagcloud" 80.0 200.0 tags <> defaultContext
    match (fromList ["about.md", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" baseCtx
            >>= relativizeUrls
        -- build up tags

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
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
                      `mappend` listField "posts" postCtx (return posts)
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
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Главная"            `mappend`
                    baseCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateFieldWith rusTimeLocale "date" "%e-го %B %Y-го года." `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

rusTimeLocale :: TimeLocale
rusTimeLocale =  TimeLocale {
        wDays  = [("Sunday",   "Sun"),  ("Monday",    "Mon"),
                  ("Tuesday",  "Tue"),  ("Wednesday", "Wed"),
                  ("Thursday", "Thu"),  ("Friday",    "Fri"),
                  ("Saturday", "Sat")],

        months = [("января",   "Jan"), ("февраля",  "Feb"),
                  ("марта",    "Mar"), ("апреля",   "Apr"),
                  ("мая",      "May"), ("июня",     "Jun"),
                  ("июля",     "Jul"), ("августа",  "Aug"),
                  ("сентября", "Sep"), ("октября",  "Oct"),
                  ("ноября",   "Nov"), ("декабря",  "Dec")],

        amPm = ("AM", "PM"),
        dateTimeFmt = "%b %a %e %H:%M:%S %Z %Y",
        dateFmt = "%m/%d/%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%I:%M:%S %p",
        knownTimeZones =
            [
            TimeZone 0 False "UT",
            TimeZone 0 False "GMT",
            TimeZone (-5 * 60) False "EST",
            TimeZone (-4 * 60) True "EDT",
            TimeZone (-6 * 60) False "CST",
            TimeZone (-5 * 60) True "CDT",
            TimeZone (-7 * 60) False "MST",
            TimeZone (-6 * 60) True "MDT",
            TimeZone (-8 * 60) False "PST",
            TimeZone (-7 * 60) True "PDT"
            ]
        }
