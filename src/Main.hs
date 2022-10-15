--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


--------------------------------------------------------------------------------
import          Data.Monoid     ((<>))
import          Hakyll


--------------------------------------------------------------------------------
-- Config
config :: Configuration
config = defaultConfiguration
    { tmpDirectory = "_tmp"
    }

-- Entrypoint
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "pages/*.md" $ do
        -- Result written to `<destination-directory>/about.html'
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
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
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
                    listField "posts" postCtx (return posts) <>
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
