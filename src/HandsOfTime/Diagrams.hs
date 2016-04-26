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

clockWidth = mkWidth 500

diagDir    = "diagrams/"


mkBase n   = trailVertices $ reverseLocTrail $ rotateBy rotateAmt $ regPoly n 1 where
  n'        = fromIntegral n
  rotateAmt = if | n == 5 -> 2 / 5
                 | odd n  -> (fromIntegral $ floor (n' / 2) - 1) * (1 / n')
                 | odd (floor $ (n' - 2) / 2) -> (1 / 4) + 1 / (2 * n')
                 | otherwise    -> (1 / 4) + 1 / n'

node :: Int -> Diagram B
node n =
  text (show n) # fontSizeL 0.2
                # fc black <> circle 0.2
                # named (show n)
                

mkClock :: [Int] -> Diagram B
mkClock xs = atPoints (mkBase $ length xs) $ fmap node xs

mkSolutions :: [[Int]] -> [Diagram B]
mkSolutions solutions = fmap mkClock' solutions where
  mkClock' positions  = mkClock positions # applyAll [connectOutside' arrOpts (show p) (show $ p + 1) | p <- positions]

                                                       
arrOpts = with & gaps .~ small

-- adds the file extension!
renderInitial :: [Int] -> FilePath -> IO FilePath
renderInitial xs path = (renderSVG (diagFileName path) clockWidth $ mkClock xs) >> return (diagFileName path)

renderSolutions :: [[Int]] -> IO [FilePath]
renderSolutions solutions =
  mapM (\(dia, n) -> renderSVG (solutionName n) clockWidth dia >> return (solutionName n)) $ zip (mkSolutions solutions) [0..]
  where
    solutionName n = diagFileName $ "solution" <> show n

renderTogether :: FilePath -> [Int] -> [[Int]] -> IO ()
renderTogether path clock solutions = renderSVG (diagFileName path) clockWidth diagram where
  diagram = foldr1 (===) $ mkClock clock : mkSolutions solutions

diagFileName :: FilePath -> FilePath
diagFileName path = diagDir `mappend` path `mappend` ".svg"
