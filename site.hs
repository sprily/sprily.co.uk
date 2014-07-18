--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import qualified Text.HTML.TagSoup as TS
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "javascript/**" $ do
        route   idRoute
        compile copyFileCompiler

    match ("css/**" .&&. (complement "css/vendor/fonts/**")) $ do
        route   idRoute
        compile compressCssCompiler

    match "css/vendor/fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
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
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            recentPosts <- loadTop 3 "posts/*"
            let indexCtx =
                    listField "recentPosts" postCtx (return recentPosts) `mappend`
                    constField "title" "Home"                            `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    previewField "preview"       `mappend`
    defaultContext

-- Top N recent posts, ordered by date
loadTop :: Int -> Pattern -> Compiler [Item String]
loadTop n s = fmap (take n) (loadAll s >>= recentFirst)

-- Extracts the first <p> ... </p> from the item
previewField :: String -> Context String
previewField key = field key $ return . prefix
        where prefix = TS.renderTags .
                       takeWhile (TS.~/= TS.TagClose ("p" :: String)) .
                       dropWhile (TS.~/= TS.TagOpen ("p" :: String) []) .
                       TS.parseTags . itemBody
