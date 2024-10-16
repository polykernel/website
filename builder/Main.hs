--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


--------------------------------------------------------------------------------
import Text.Pandoc.Highlighting (Style, haddock, styleToCss)
import Hakyll


--------------------------------------------------------------------------------
-- Config
config :: Configuration
config = defaultConfiguration
    { tmpDirectory = "_tmp"
    }

pandocHighlightingStyle :: Style
pandocHighlightingStyle = haddock

-- Entrypoint
main :: IO ()
main = hakyllWith config $ do
    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Generate syntax highlighting stylesheet for pandoc
    -- courtesy of Rebecca Skinner (https://rebeccaskinner.net/posts/2021-01-31-hakyll-syntax-highlighting.html)
    create ["css/style.css"] $ do
        route idRoute
        compile $ makeItem $ styleToCss pandocHighlightingStyle

    match "pages/*.md" $ do
        -- Result written to `<destination-directory>/<page-name>.html'
        route   $ composeRoutes (gsubRoute "pages/" (const "")) (setExtension "html")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let
                posts = recentFirst =<< loadAll "posts/*"
                archiveCtx =
                    listField "posts" postCtx posts <>
                    constField "title" "Archives"   <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let
                posts = recentFirst =<< loadAll "posts/*"
                indexCtx =
                    listField "posts" postCtx posts <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls


    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
-- Contexts
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
