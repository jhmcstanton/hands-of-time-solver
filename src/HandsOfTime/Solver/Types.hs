{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE BangPatterns               #-}
module HandsOfTime.Solver.Types
       (
         Hands (..),
         InitBoard,
         Board,
         mkPuzzle,
         Puzzle (..)
       )
where

import           Data.Vector

data Hands =
  Hands {
      counterClock :: !Int,
      clock        :: !Int
    } deriving (Eq, Ord, Show)

type InitBoard = Puzzle Int
type Board     = Puzzle (Maybe Int)

mkPuzzle :: [Int] -> Puzzle Int
mkPuzzle xs = Puzzle 0 (Hands 0 0) (fromList xs)

data Puzzle :: * -> * where
  Puzzle  :: {
    steps :: !Int,
    hands :: !Hands,
    -- starts from 12 o clock then input clockwise
    board :: !(Vector a)
  } -> Puzzle a

instance Functor Puzzle where
  fmap f puzzle = puzzle { board = fmap f (board puzzle) }


