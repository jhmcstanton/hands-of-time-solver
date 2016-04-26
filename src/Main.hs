{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

import           Web.Scotty
import           Network.Wai.Middleware.Static
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text
import           Data.Monoid
import           Data.Text.Lazy (pack)
import           Control.Monad.IO.Class
import           System.Directory (createDirectoryIfMissing)

import           HandsOfTime.Solver
import           HandsOfTime.Solver.Types (mkPuzzle)
import           HandsOfTime.Diagrams


portNumber  = 3000

main = do
  createDirectoryIfMissing False diagDir
  scotty portNumber $ do
    middleware $ staticPolicy (noDots >-> addBase "")
    get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      html $ renderHtml index
    get "/solutions" $ do
      clock <- param "clock" 
      let clockVals = fmap read $ words $ fmap (\case { '+' -> ' '; x -> x }) clock
          clock'    = mkPuzzle clockVals 
          solutions = solve $ clock'
      clockPath     <- liftIO $ renderInitial clockVals "clock"
      solutionPaths <- liftIO $ renderSolutions solutions
      html . renderHtml $ mkSolutionPage clockPath solutionPaths

mkSolutionPage :: FilePath -> [FilePath] -> H.Html
mkSolutionPage clockFile solutions = H.docTypeHtml $ (H.head $ H.title "Hands of Time Solver!") <> (H.body $ clockHtml <> solutionsHtml)
  where
    clockHtml     = H.h1 (H.toHtml ("Provided Clock" :: String)) <> H.br <> H.img H.! A.src (H.stringValue clockFile) H.! imgStyle <> H.br
    solutionsHtml =
      if null solutions
         then H.h1 $ H.toHtml ("No solutions found, did you input the clock correctly?" :: String)
         else H.h1 (H.toHtml ("All solutions found:" :: String)) <> H.br <> imgTags
    imgTags       = foldr (\filename html -> (H.img H.! A.src (H.stringValue filename) H.! imgStyle ) <> html) mempty solutions
    imgStyle      = A.style $ H.textValue "display:block;margin:auto"

index :: H.Html
index = H.docTypeHtml $ (H.head $ H.title "Hands of Time Solver!") <> (H.body (appHeader <> clockForm)) where
  appHeader    = H.h1 $ H.toHtml ("Hands of Time Solver!" :: String)
  clockForm    = H.form H.! A.action "/solutions" H.! A.method "get" $ formContents
  formContents = (H.label H.! A.for "clock" $ H.toHtml ("Clock Values:" :: String))
    <> (H.input H.! A.type_ "text" H.! A.name "clock" H.! A.placeholder "1 2 3 4 5 6")
    <> H.br
    <> (H.button H.! A.type_ "submit" $ H.toHtml ("Solve!" :: String))
