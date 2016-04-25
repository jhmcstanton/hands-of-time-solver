{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
module HandsOfTime.Diagrams
       (
         renderInitial,
         renderSolutions
       )
where

import           Diagrams.Prelude
import           Diagrams.Backend.SVG

import           Data.Monoid (mappend)

clockWidth = mkWidth 500

diagDir    = "diagrams/"

mkBase n   = trailVertices $ reverseLocTrail $ regPoly n 1

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
  --[connectPerim' arrOpts (show p) (show $ p + 1) (2/12 @@ turn) (4/12 @@ turn) | p <- positions]

                                                       
arrOpts = with & gaps .~ small

-- adds the file extension!
renderInitial :: [Int] -> FilePath -> IO ()
renderInitial xs path = renderSVG (diagFileName path) clockWidth $ mkClock xs

--                            
renderSolutions :: [[Int]] -> IO ()
renderSolutions solutions =
  mapM_ (\(dia, n) -> renderSVG (diagFileName $ "solution" <> show n) clockWidth dia) $ zip (mkSolutions solutions) [0..]

diagFileName :: FilePath -> FilePath
diagFileName path = diagDir `mappend` path `mappend` ".svg"
