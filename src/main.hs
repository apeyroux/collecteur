{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified Data.Text.IO as IOT
import qualified Data.Text as T
import Collecteur.LBC

main :: IO()
main = do
  (url:_) <- getArgs
  annonces <- collectResult url
  mapM_ (\a -> IOT.putStrLn $ (T.append (T.append (lbcTitle a) " - ")) (lbcLink a)) annonces
    
