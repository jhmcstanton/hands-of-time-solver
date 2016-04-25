{-# LANGUAGE LambdaCase #-}
module HandsOfTime.Solver
       (
       )
where

import           HandsOfTime.Solver.Types

import qualified Data.Vector as V
import           Data.Monoid
import           Data.Maybe (fromJust, isJust)

type Positions = [Int]

testPuz = mkPuzzle [2, 3, 2, 3, 3, 3, 1, 4]

-- takes a position in the vector and uses that to fetch the distance hands should move
move :: Int -> Board -> Maybe Board
move index puzzle = do
  moveAmount <- board puzzle V.! index -- getting a maybe index, not a safe return!
  let boardLength = V.length . board $ puzzle
--  let newHands = Hands ((counterClock (hands puzzle) - moveAmount) `mod` boardLength) ((clock (hands puzzle) + moveAmount) `mod` boardLength)
  let newHands = Hands ((index - moveAmount) `mod` boardLength) ((index + moveAmount) `mod` boardLength)
  let newBoard = V.update (board puzzle) (V.fromList [(index, Nothing)])
  return $ puzzle { hands = newHands, board = newBoard, steps = steps puzzle + 1 }

bothMoves :: Board -> (Maybe Board, Maybe Board)
bothMoves puz@(Puzzle _ (Hands counterClock clock) _)
  | counterClock == clock = (move clock puz, Nothing)
  | otherwise             = (move counterClock puz, move clock puz)


solve :: InitBoard -> [Positions]
solve puzzle = concat $ zipWith solve' [ Just x | x <- [1..V.length (board puzzle) - 1]] (repeat $ fmap Just puzzle)

solve' :: Maybe Int -> Board -> [Positions]
solve' (Just start) puz =
  case updatePositions start puz' of
    Nothing      -> []
    Just results -> results
  where
    puz' = move start puz
solve' Nothing puz@(Puzzle _ (Hands counterClock clock) _)
  | steps puz == V.length (board puz) - 1 = [[ fromJust . fromJust $ V.find isJust (board puz)]] -- done!
  | otherwise = maybeConcat (updatePositions counterClock counterPuz) (updatePositions clock forwardPuz)
    where
      (counterPuz, forwardPuz)  = bothMoves puz

updatePositions :: Functor f => Int -> f Board -> f [[Int]]
updatePositions pos = fmap (fmap (pos :) . solve' Nothing)

maybeConcat :: Maybe [a] -> Maybe [a] -> [a]
maybeConcat (Just xs) (Just ys) = xs <> ys
maybeConcat (Just xs) Nothing   = xs
maybeConcat Nothing   (Just ys) = ys
maybeConcat Nothing   Nothing   = []
  
