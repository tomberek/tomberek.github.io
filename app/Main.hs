{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Options

main :: IO ()
main =
  hakyll $ do
    match (fromList ["index.html","*.html","resume/*.html"]) $ do
      route idRoute
      compile $ getResourceBody >>= relativizeUrls

    match ("robots.txt" .||. "images/**" .||. "resume/*") $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
      route (setExtension "html")
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    match "posts/*" $ do
      route (setExtension "html")
      compile $ pandocCompilerWithTOC
        >>= loadAndApplyTemplate "templates/post.html" postContext
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postContext
        >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveContext =
              listField "posts" postContext (return posts) `mappend`
              constField "title" "Archives" `mappend`
              defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveContext
          >>= loadAndApplyTemplate "templates/default.html" archiveContext
          >>= relativizeUrls

    match "blog.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexContext =
              listField "posts" postContext (return posts) `mappend`
              constField "title" "Home" `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexContext
          >>= loadAndApplyTemplate "templates/default.html" indexContext
          >>= relativizeUrls

    match "templates/*" (compile templateCompiler)

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take 10) . recentFirst
                   =<< loadAllSnapshots "posts/*" "content"
        renderAtom feedConfiguration feedContext posts

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take 10) . recentFirst
                   =<< loadAllSnapshots "posts/*" "content"
        renderRss feedConfiguration feedContext posts

pandocCompilerWithTOC = do 
      ident <- getUnderlying 
      toc    <- getMetadataField ident "toc" 
      let writerSettings = case toc of 
                                Just "yes"  -> myWriterOptionsToc 
                                Nothing     -> myWriterOptions 
      pandocCompilerWith defaultHakyllReaderOptions writerSettings 

myWriterOptions :: WriterOptions 
myWriterOptions = defaultHakyllWriterOptions { 
      writerReferenceLinks = True 
    , writerHtml5 = True 
    , writerHighlight = True 
    } 

myWriterOptionsToc :: WriterOptions 
myWriterOptionsToc = myWriterOptions { 
      writerTableOfContents = True 
    , writerTOCDepth = 2 
    , writerTemplate = "$if(toc)$<div id=\"toc\">$toc$</div>$endif$\n$body$" 
    , writerStandalone = True 
    } 

feedContext :: Context String
feedContext =
  postContext `mappend` bodyField "description"

postContext :: Context String
postContext =
  dateField "date" "%Y-%m-%d" `mappend` defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "tomberek's Blog"
    , feedDescription = "tomberek's blog"
    , feedAuthorName = "tomberek"
    , feedAuthorEmail = "tomberek@gmail.com"
    , feedRoot = ""
    }
