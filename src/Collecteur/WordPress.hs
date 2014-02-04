{-# LANGUAGE OverloadedStrings #-}

import Collecteur.Internal

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, attribute, content, element, fromDocument, child,
                        ($//), (&/), (&|), (&//), (>=>))

data Article = Article {
  wpTitle :: String,
  wpContent :: String
  } deriving Show

findArticleTitle :: Cursor -> [Cursor]
findArticleTitle = element "h3" >=> attributeIs "class" "title"  >=> child &// element "a"  >=> child

findArticleContent :: Cursor -> [Cursor]
findArticleContent = element "div" >=> attributeIs "class" "postbody entry" &/ element "p"

-- Extract the data from each node in turn
extractDataArticleTitle :: Cursor -> T.Text 
extractDataArticleTitle = T.strip . T.concat . content

extractDataArticleContent :: Cursor -> T.Text 
extractDataArticleContent = T.strip . T.concat . attribute "href"

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- simpleHttp u
     return $ fromDocument $ parseLBS page

extractWP :: String -> IO [Article]
extractWP url = do
  cursor <- cursorFor url
  let annonces = zip (cursor $// findArticleTitle &| extractDataArticleTitle) (cursor $// findArticleContent &| extractDataArticleContent)
  return $ map (\(l,t) -> Article l t) annonces

collectResult :: String -> IO [Article]
collectResult url = extractWP url
