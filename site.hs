--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Control.Monad
import           Hakyll
import           Data.Text.Lazy.IO (writeFile)
import           Data.Time
import           Clay
import           Prelude hiding (div, writeFile)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- собираем css
  writeFile "css/default.css" defaultCss
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")    
    let baseCtx = tagCloudField "tagcloud" 80.0 200.0 tags <> defaultContext
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

-- background black
-- width     (px 600)

                  
defaultCss = render $ do
    body ? do
        backgroundImage . url $ "https://raw.githubusercontent.com" <> 
            "/vojiranto/vojiranto.github.io/master/images/f.jpg"
        fontSize  (px  16)
        margin    (px   0) auto (px 0) auto
    
    let -- оформление края блока.
        defaultBorder = do
            borderRadius (px 5) (px 5) (px 5) (px 5)
            border       solid    (px 0.5)  (grayish $ 160)

                                   {-top-}  {-right-} {-bottom-} {-left-}
        defaultPadding   = padding (em 1)   (em 1)    (em 1)     (em 1)

    forM [(".navigation", px 40), (".tagcloud", em 0.5)] $
      (\(elem, t) -> elem ? do
        background    white
        defaultBorder
        defaultPadding
        fontSize     (px 20)
        margin       t (em 0.5)  (em 0.5)   (em 0))

    ".sidebar" ? do
        position fixed
        width  (em 10)
        height (pt 100)

    ".myBody" ? do
        background    white
        defaultBorder
        defaultPadding
        fontSize     (px 16)
        width        (em 40)
        margin       (em 0) (em 0) (em 0) (em 10)
    ".header_fix" ? do
        zIndex 1
        defaultBorder
        background (grayish $ 230)
        position fixed
        width        (pc 100)
        fontSize (px 30)
        margin  (em 0) (em 0) (em 0) (em 0)
        padding (em 0)   (em 0)    (em 0)     (em 0)

{-    div#header_fix {
        z-index: 1;
        padding: 5px 20px 20px 20px;
        position: fixed;
        border-radius: 5px;
        font-size: 30px; 
    	background-color: #EEE;
        border: 0.5px solid #BBB;
        width:100%;
        height: 20px;
        font-weight: bold;
    }-}

    div # "#content" ? h1 ? do
        borderBottom solid (px 2) black

    footer ? do
        color     "#555"
        fontSize  (px 12)
        marginTop (px 30)
        padding   (px 12) (px 0) (px 12) (px 0)
        textAlign (alignSide sideCenter)

    h1 ? do
        fontSize (px 24)
    h2 ? do
        fontSize (px 20)
