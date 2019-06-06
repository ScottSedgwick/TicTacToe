module Ai where

import Data.Tree
import TicLib

data ScoreState = ScoreState (State, Integer)

instance Eq ScoreState where
  (==) (ScoreState (_,x)) (ScoreState (_,y)) = x == y

instance Ord ScoreState where
  compare (ScoreState (_,x)) (ScoreState (_,y)) = compare x y

class Negatable a where
  neg :: a -> a

instance Negatable ScoreState where
  neg (ScoreState (s,x)) = ScoreState (s, negate x)

-- Scoring function
score :: State -> ScoreState
score = undefined

bestMove :: State -> Move
bestMove = bestMoveToDepth 9

bestMoveToDepth :: Int -> State -> Move
bestMoveToDepth d s = getMove (score s) s'
  where
    s' = minimax_ab minState maxState (pruneTree d (buildTree (score s)))

minState :: ScoreState
minState = ScoreState (State [], -10)

maxState :: ScoreState
maxState = ScoreState (State [], 10)

-- | Get the next move to make from a minimax State
--
-- Examples:
--
-- >>> getMove (ScoreState (State [(NW, Cross), (N,Cross), (NE,Cross)], 0)) (ScoreState (State [(SE,Nought),(NW, Cross), (N,Cross), (NE,Cross)], 0))
-- (SE,O)
getMove :: ScoreState -> ScoreState -> Move
getMove (ScoreState (State ix, _)) (ScoreState (State rx, _)) = head (drop (length ix) (reverse rx))

pruneTree :: Int -> Tree a -> Tree a
pruneTree = undefined

buildTree :: ScoreState -> Tree ScoreState
buildTree = undefined

-- minimax with alpha-beta prunning
minimax_ab :: (Negatable a, Ord a) => a -> a -> Tree a -> a
minimax_ab a b (Node x []) = a `max` x `min` b
minimax_ab a b (Node x ts) = cmx a ts
    where cmx a []  = a
          cmx a (t:ts) | a' >= b   = a'
                       | otherwise = cmx a' ts
                       where a' = neg (minimax_ab (neg b) (neg a) t)
