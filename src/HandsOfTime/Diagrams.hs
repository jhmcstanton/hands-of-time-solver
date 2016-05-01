{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiWayIf                #-}
module HandsOfTime.Diagrams
       (
         renderInitial,
         renderSolutions,
         diagDir
       )
where

import           Diagrams.Prelude
import           Diagrams.Backend.SVG

import           Data.Monoid (mappend)
import           Data.Maybe  (fromJust, fromMaybe)
import           Data.List   (sortBy, elemIndex)


clockWidth = mkWidth 500

diagDir    = "diagrams/"


mkBase n    = trailVertices $ reverseLocTrail $ rotateBy rotateAmt $ regPoly n 1 where
  n'        = fromIntegral n
  rotateAmt = if | n == 5 -> 2 / 5
                 | odd n  -> (fromIntegral $ floor (n' / 2) - 1) * (1 / n')
                 | odd (floor $ (n' - 2) / 2) -> (1 / 4) + 1 / (2 * n')
                 | otherwise                  -> (1 / 4) + 1 / n'

node :: Int -> Int -> Diagram B
node n nameInt =
  text (show n) # fontSizeL 0.2
                # fc black <> circle 0.2
                # named (show nameInt)
                

mkClock :: [Int] -> [Int] -> Diagram B
mkClock xs names = atPoints (mkBase $ length xs) $ zipWith node xs names

mkSolutions :: Maybe [Int] -> [[Int]] -> [Diagram B]
mkSolutions origPath solutions = fmap mkClock' solutions where
  mkClock' positions  = mkClock positions' names # applyAll [connectOutside' arrOpts (show p) (show $ (p + 1)) | p <- positions]
    where
      names      = zipWith (\n ps -> fromJust $ elemIndex n ps) [0..length positions - 1] (repeat positions)
      positions' = fromMaybe names origPath
                                                       
arrOpts = with & gaps .~ small

-- adds the file extension!
renderInitial :: [Int] -> FilePath -> IO FilePath
renderInitial xs path = (renderSVG (diagFileName path) clockWidth $ mkClock xs xs) >> return (diagFileName path)

renderSolutions :: Maybe [Int] -> [[Int]] -> IO [FilePath]
renderSolutions origPath solutions =
  mapM (\(dia, n) -> renderSVG (solutionName n) clockWidth dia >> return (solutionName n)) $ zip (mkSolutions origPath solutions) [0..]
  where
    solutionName n = diagFileName $ "solution" <> show n

renderTogether :: Maybe [Int] -> FilePath -> [Int] -> [[Int]] -> IO ()
renderTogether origPath path clock solutions = renderSVG (diagFileName path) clockWidth diagram where
  diagram = foldr1 (===) $ mkClock clock clock : mkSolutions origPath solutions 

diagFileName :: FilePath -> FilePath
diagFileName path = diagDir `mappend` path `mappend` ".svg"
