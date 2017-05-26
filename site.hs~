--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Time

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
{-
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
-}
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Архив"               `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Главная"            `mappend`
                    defaultContext

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
