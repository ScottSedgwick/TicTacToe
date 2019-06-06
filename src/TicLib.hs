{-|
Module      : TicLib
Description : Implementation of Tony Morris' TicTacToe exercise
Copyright   : (c) Scott Sedgwick (2019)
License     : BSD3
Maintainer  : scott.sedgwick@gmail.com
Stability   : experimental
Portability : POSIX

This is my implementation of the [TicTacToe](https://github.com/tonymorris/fp-course/blob/master/projects/TicTacToe/TicTacToe.markdown) exercise in Tony Morris' fp-course.

I have attempted to fulfil all the requirements, with this exception - rather than use CABAL and include a .ghci file, I built with Stack.
-}
module TicLib 
    ( FindPlayerAt
    , Game (..)
    , Mark (..)
    , Move
    , Playable (Playable)
    , Played (Played)
    , Posn (..)
    , State (..)
    , initState
    , isDraw
    , move
    , playerAt
    , takeBack
    , whoWon
    ) where

import Data.List (drop, find, intercalate, take)

class NextMove a where
    next :: a -> Mark

class PositionOccupied a where
    positionIsOccupied :: Posn -> a -> Bool

-- | Instances of this class can return what player is at a specified position
class FindPlayerAt a where
    -- | Returns the player at the specified `Posn` in the game
    playerAt :: Posn -> a -> Mark

class FindWhoWon a where
    -- | Returns who won the game.
    -- | If the game is not complete or is drawn, returns `None`.
    whoWon :: a -> Mark

-- | Represents whose turn it is.
data Mark = None | Nought | Cross deriving (Eq)
instance Show Mark where
  show None   = " "
  show Nought = "O"
  show Cross  = "X"

-- | A type that enumerates all the places on the board.
data Posn = NW | N | NE
          |  W | C |  E
          | SW | S | SE
          deriving (Eq, Ord, Enum, Show)

-- | Represents a single move (where, and who)
type Move = (Posn, Mark)

-- | Represents the state of the board - an ordered collection of `Move`s
newtype State = State [Move]
              deriving (Show, Eq)

instance NextMove State where
    next (State [])             = Cross
    next (State ((_,None  ):_)) = None
    next (State ((_,Nought):_)) = Cross
    next (State ((_,Cross ):_)) = Nought

instance PositionOccupied State where
    positionIsOccupied p (State xs) = any (\t -> p == fst t) xs

instance FindPlayerAt State where
    playerAt p (State xs) = case find (\(p',_) -> p' == p) xs of
                                Nothing      -> None
                                (Just (_,m)) -> m

conc :: Move -> State -> State
conc x (State xs) = State (x:xs)

-- | A top-level data structure that can serve as the complete state for Elm style application frameworks.
data Game = ToPlay Playable
          | FromPlay Played

instance NextMove Game where
    next (ToPlay x)   = next x
    next (FromPlay x) = next x

instance FindPlayerAt Game where
    playerAt p (ToPlay x)   = playerAt p x
    playerAt p (FromPlay x) = playerAt p x

instance FindWhoWon Game where
    whoWon (ToPlay x)   = whoWon x
    whoWon (FromPlay x) = whoWon x

-- | The initial state of a new game.
initState :: Game
initState = ToPlay EmptyBoard

-- | Tests a `Game` to determine if it is drawn.
isDraw :: Game -> Bool
isDraw (FromPlay (Drawn _)) = True
isDraw _                    = False

-- | Represents a game board that can still have moves made on it.
data Playable = EmptyBoard
              | Playable State
              deriving (Eq, Show)

instance PositionOccupied Playable where
    positionIsOccupied _ EmptyBoard   = False
    positionIsOccupied p (Playable s) = positionIsOccupied p s

instance NextMove Playable where
    next EmptyBoard   = Cross
    next (Playable s) = next s

instance FindPlayerAt Playable where
    playerAt _ EmptyBoard   = None
    playerAt p (Playable x) = playerAt p x

instance FindWhoWon Playable where
    whoWon _ = None

-- | Represents a game board that has had a move made.
-- | The Played constructor represents an instance of this type that can still have more moves made.
data Played = Played State
            | Drawn State
            | Won State

instance NextMove Played where
    next (Played s)   = next s
    next _            = None

instance FindPlayerAt Played where
    playerAt p (Played xs) = playerAt p xs
    playerAt p (Won    xs) = playerAt p xs
    playerAt p (Drawn  xs) = playerAt p xs

instance FindWhoWon Played where
    whoWon (Played _)              = None
    whoWon (Drawn _)               = None
    whoWon (Won (State ((_,m):_))) = m

startBoard :: (Mark, Playable)
startBoard = (Nought, EmptyBoard)

-- | The function you call to make a move.
-- | If the move is invalid (the position is occupied, or it is not your turn) then this does nothing and an unchanged game state will be returned.
move :: (Posn, Playable) -> Played
move (p, EmptyBoard) = Played (State [(p, next EmptyBoard)])
move (p, Playable s) | positionIsOccupied p s = Played s
                     | otherwise              = move' ((p,m) `conc` s)
    where
        m = next s
        move' ys | hasWon ys = Won ys 
                 | isFull ys = Drawn ys
                 | otherwise = Played ys

winningMoves :: [[Posn]]
winningMoves = 
    [ [ NW, N, NE ], [  W, C,  E ], [ SW, S, SE ]   -- Rows
    , [ NW, W, SW ], [  N, C,  S ], [ NE, E, SE ]   -- Columns
    , [ NW, C, SE ], [ NE, C, SW ]                  -- Diagonals
    ]

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll ns xs = all (`elem` xs) ns

-- | Determine if a game has been won
--
-- Examples:
--
-- >>> hasWon (State [(NW, Cross), (N,Cross), (NE,Cross)])
-- True
hasWon :: State -> Bool
hasWon (State []) = False
hasWon (State xs) = hasWon' (map fst (filter (\(_,m') -> m == m') xs))
  where
    m = snd (head xs)
    hasWon' ys = any (`containsAll` ys) winningMoves

isFull :: State -> Bool
isFull (State xs) = length xs >= 9

-- $setup
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary Posn     where arbitrary = elements [ NW .. SE ]
-- >>> instance Arbitrary Mark     where arbitrary = elements [ Nought, Cross ]
-- >>> instance Arbitrary Playable where arbitrary = Playable <$> State <$> arbitrary
-- >>> empty (Playable (State xs)) = xs == []

-- | Takes back the last turn in a game.
--
-- Example:
--
-- prop> \b p -> empty b || positionIsOccupied p b || takeBack (move (p, b)) == b
takeBack :: Played -> Playable
takeBack (Played (State [_]   )) = EmptyBoard
takeBack (Played (State (_:xs))) = Playable (State xs)
takeBack (Won    (State [_]   )) = EmptyBoard
takeBack (Won    (State (_:xs))) = Playable (State xs)
takeBack (Drawn  (State [_]   )) = EmptyBoard
takeBack (Drawn  (State (_:xs))) = Playable (State xs)
