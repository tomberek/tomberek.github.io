{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Options

main :: IO ()
main =
  hakyll $ do
    match (fromList ["matrix.html","editor.html","markdeep.html","markdeep-editor.html","markdeep-viewer.html","resume/*.html"]) $ do
      route idRoute
      compile $ getResourceBody >>= relativizeUrls

    match ("favicon.ico" .||. "googleefc9de7add097f6e.html" .||. "editr/**" .||. "robots.txt" .||. "keybase.txt" .||. "images/**" .||. "resume/*") $ do
      route idRoute
      compile copyFileCompiler

    match "js/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match (fromList ["about.md", "contact.md", "index.md"]) $ do
      route (setExtension "html")
      compile $ pandocCompilerWith defaultHakyllReaderOptions myWriterOptions
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    tags <- buildTags "posts/**" (fromCapture "tags/*.html")
    categories <- buildCategories "posts/**" (fromCapture "categories/*.html")
    let tagsRules' t s = tagsRules t $ \tag pattern -> do
            let title = s ++ " \"" ++ tag ++ "\""
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern
                let ctx = constField "title" title
                          `mappend` listField "posts" postContext (return posts)
                          `mappend` defaultContext
    
                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls
    tagsRules' categories "Posts in category"
    tagsRules' tags "Posts tagged"

    match "posts/**" $ do
      route (setExtension "html")
      compile $ pandocCompilerWithTOC
        >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags categories)
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags categories)
        >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/**"
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
        posts <- recentFirst =<< loadAll "posts/**"
        let indexContext =
              listField "posts" postContext (return posts) `mappend`
              constField "title" "Blog" `mappend`
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
                   =<< loadAllSnapshots "posts/**" "content"
        renderAtom feedConfiguration feedContext posts

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take 10) . recentFirst
                   =<< loadAllSnapshots "posts/**" "content"
        renderRss feedConfiguration feedContext posts

pandocCompilerWithTOC = do
      ident <- getUnderlying
      toc    <- getMetadataField ident "toc"
      let writerSettings = case toc of
                                Just "yes"  -> myWriterOptionsToc
                                _           -> myWriterOptions
      pandocCompilerWith defaultHakyllReaderOptions writerSettings

postCtxWithTags :: Tags -> Tags -> Context String
postCtxWithTags tags categories = tagsField "tags" tags
                      `mappend` categoryField "categories" categories
                      `mappend` postContext

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions {
      writerReferenceLinks = True,
      writerSectionDivs = True,
      writerNumberSections = True
    }

myWriterOptionsToc :: WriterOptions
myWriterOptionsToc = myWriterOptions {
      writerTableOfContents = True
    , writerTOCDepth = 2
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
