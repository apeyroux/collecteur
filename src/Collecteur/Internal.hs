{-# LANGUAGE OverloadedStrings #-}

module Collecteur.Internal where

--import Collecteur.WordPress

data Subscription = VBulletin | WordPress | Twitter deriving Show
data Refresh = Hourly | Daily | Random deriving Show

data CollecteConfiguration = CollecteConfiguration {
  tsub :: Subscription,
  trefresh :: Refresh } deriving Show

data CollecteResult = CollecteResult {
  crContent :: String
  } deriving Show

defaultConfig :: CollecteConfiguration
defaultConfig = CollecteConfiguration WordPress Hourly

