{-# LANGUAGE OverloadedStrings #-}

module Collecteur.LBC (
  Annonce,
  lbcTitle,
  lbcLink,
  collectResult
  ) where

import Collecteur.Internal

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, attribute, content, element, fromDocument, child,
                        ($//), (&/), (&|), (&//), (>=>))

data Annonce = Annonce {
  lbcLink :: T.Text,
  lbcTitle :: T.Text
  } deriving Show

findAds :: Cursor -> [Cursor]
findAds = element "div" >=> attributeIs "class" "lbc"  >=> child &// element "div" >=> attributeIs "class" "title" >=> child

findAdsLinks :: Cursor -> [Cursor]
findAdsLinks = element "div" >=> attributeIs "class" "list-lbc" &/ element "a"

-- Extract the data from each node in turn
extractData :: Cursor -> T.Text 
extractData = T.strip . T.concat . content

extractDataLinks :: Cursor -> T.Text 
extractDataLinks = T.strip . T.concat . attribute "href"

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- simpleHttp u
     return $ fromDocument $ parseLBS page

extractLBC :: String -> IO [Annonce]
extractLBC url = do
  cursor <- cursorFor url
  let annonces = zip (cursor $// findAdsLinks &| extractDataLinks) (cursor $// findAds &| extractData)
  return $ map (\(l,t) -> Annonce l t) annonces

collectResult :: String -> IO [Annonce]
collectResult url = extractLBC url
