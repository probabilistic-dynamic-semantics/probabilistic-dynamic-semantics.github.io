--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Data.String
import Hakyll
import Hakyll.Core.Identifier
import Text.Pandoc
import Text.Pandoc.Walk (walk)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match ("css/styles.css"
         .||. "images/*"
        ) $ do
    route idRoute
    compile copyFileCompiler
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match (fromList (
            map (fromFilePath . (++ '.' : format) . streamline) sections
            )) $ do
    route $ setExtension "html"
    compile $ do
      item1 <- pandocCompilerHandlingAbstract
      item2 <- loadAndApplyTemplate
               "templates/layout.html"
               (defaultContext `mappend` navContext sections)
               item1
      relativizeUrls item2
  match "templates/*" $ compile templateBodyCompiler
  

--------------------------------------------------------------------------------
sections :: [String]
sections = [ "about"
           , "components"
           , "writing"
           , "documentation"
           , "installation"
           , "quick start"
           ]

streamline :: String -> String
streamline s = filter (/= ' ') $ if s == "about" then "index" else s

format :: String
format = "md"

navString :: [String] -> String -> String
navString sections currentPage
  = concat $ map (\(str, labels) -> if currentPage `elem` labels
                                    then "<li class=\"active\">"
                                         ++ str
                                         ++ "</li>"
                                    else if null labels
                                         then str
                                         else "<li>" ++ str ++ "</li>")
    (("<div class=\"navbar-collapse collapse\">\n"
       ++ "<ul class=\"nav navbar-nav\">", [])
    : map (\s -> ("<a href=\"" ++ streamline s ++ ".html\">" ++ s ++ "</a>", [s])) sections
    ++ [ ("</ul>\n</div>", []) ])

navContext :: [String] -> Context a
navContext sections = functionField "nav" f
  where f [ttl] _ = return $ navString sections ttl

pandocCompilerHandlingAbstract :: Compiler (Item String)
pandocCompilerHandlingAbstract = do
  let readerOpts = defaultHakyllReaderOptions 
        { readerExtensions = enableExtension Ext_fenced_code_blocks 
          $ readerExtensions defaultHakyllReaderOptions }
  pandoc <- readPandocWith readerOpts =<< getResourceBody
  let transformedPandoc = transformAbstract (itemBody pandoc)
  return $ writePandocWith defaultHakyllWriterOptions (pandoc { itemBody = transformedPandoc })

-- Transform `Div ("",["abstract"],[])` into `<details><summary>`
transformAbstract :: Pandoc -> Pandoc
transformAbstract = walk transformBlock
  where
    transformBlock :: Block -> Block
    transformBlock (Div (_, classes, _) content)
      | "abstract" `elem` classes =
          let summary = [Para [Str "Abstract"]]
              detailsContent = content
              detailsHtml = RawBlock "html" "<details><summary>"
                            : summary
                            ++ RawBlock "html" "</summary>" 
                            : detailsContent
                            ++ [RawBlock "html" "</details>"]
          in Div ("", [], []) detailsHtml
    transformBlock block = block
