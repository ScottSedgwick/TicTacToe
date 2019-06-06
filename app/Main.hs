module Main where

import qualified Brick as B
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BS
import Data.Char (chr)
import Data.List (intercalate)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as VA
import Control.Monad (void)

import TicLib 
  ( Game (..)
  , Playable (Playable)
  , Played (Played)
  , Posn (..)
  , initState
  , isDraw
  , move
  , playerAt
  , takeBack
  , whoWon)

type AppEvents = ()
type AppName = ()

app :: B.App Game AppEvents AppName
app = B.App 
  { B.appDraw         = drawUI
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = handleEvent
  , B.appStartEvent   = pure
  , B.appAttrMap      = \_ -> B.attrMap VA.defAttr []
  }

drawUI :: Game -> [B.Widget AppName]
drawUI g = [ B.hBox [drawInstructions, drawGame g ] ]

drawInstructions :: B.Widget AppName
drawInstructions = B.withBorderStyle BS.unicodeBold
                  $ BB.borderWithLabel (B.str " Instructions ")
                  $ B.padLeftRight 1
                  $ B.vBox [ B.str ("Moves:  q " ++ vbar ++ " w " ++ vbar ++ " e ")
                          , B.str ("       " ++ hline)
                          , B.str ("        a " ++ vbar ++ " s " ++ vbar ++ " d ")
                          , B.str ("       " ++ hline)
                          , B.str ("        z " ++ vbar ++ " x " ++ vbar ++ " c ")
                          , B.str "(Esc) to exit."
                          , B.str "(r) to restart."
                          , B.str "(t) to take back."
                          ]

hline :: String
hline = x ++ y ++ x ++ y ++ x
  where
    x = replicate 3 '\x2501'
    y = "\x254b"

vbar :: String
vbar = "\x2503"

drawGame :: Game -> B.Widget AppName
drawGame g = B.withBorderStyle BS.unicodeBold
            $ BB.borderWithLabel (B.str winMsg)
            $ B.padAll 1
            $ B.vBox [ drawRow g [NW, N, NE]
                    , B.str hline
                    , drawRow g [W, C, E]
                    , B.str hline
                    , drawRow g [SW, S, SE]
                    , B.str "           "
                    ]
  where
    winMsg = if isDraw g 
              then " It's a Draw! "
              else let s = show (whoWon g) 
                  in if s == " " 
                      then " Game "
                      else " " ++ s ++ "'s won! "


drawRow :: Game -> [Posn] -> B.Widget AppName
drawRow g ps = B.str (' ' : intercalate (' ' : vbar ++ " ") (map (\p -> show (playerAt p g)) ps))

handleEvent :: Game -> B.BrickEvent AppName AppEvents -> B.EventM AppName (B.Next Game)
handleEvent g e = 
  case e of
    B.VtyEvent (V.EvKey V.KEsc        []) -> B.halt g
    B.VtyEvent (V.EvKey (V.KChar 'q') []) -> makeMove g NW
    B.VtyEvent (V.EvKey (V.KChar 'w') []) -> makeMove g N
    B.VtyEvent (V.EvKey (V.KChar 'e') []) -> makeMove g NE
    B.VtyEvent (V.EvKey (V.KChar 'a') []) -> makeMove g W
    B.VtyEvent (V.EvKey (V.KChar 's') []) -> makeMove g C
    B.VtyEvent (V.EvKey (V.KChar 'd') []) -> makeMove g E
    B.VtyEvent (V.EvKey (V.KChar 'z') []) -> makeMove g SW
    B.VtyEvent (V.EvKey (V.KChar 'x') []) -> makeMove g S
    B.VtyEvent (V.EvKey (V.KChar 'c') []) -> makeMove g SE
    B.VtyEvent (V.EvKey (V.KChar 'r') []) -> B.continue initState
    B.VtyEvent (V.EvKey (V.KChar 't') []) -> B.continue (doTakeBack g)
    _                                     -> B.continue g

makeMove :: Game -> Posn -> B.EventM AppName (B.Next Game)
makeMove (FromPlay s) _ = B.continue (FromPlay s)
makeMove (ToPlay s)   p = B.continue $ case move (p, s) of
                                        (Played t) -> ToPlay (Playable t)
                                        p          -> FromPlay p

doTakeBack :: Game -> Game
doTakeBack (ToPlay (Playable s)) = doTakeBack (FromPlay (Played s))
doTakeBack (FromPlay played)     = ToPlay (takeBack played)
doTakeBack g                     = g

main :: IO ()
main = void $ B.defaultMain app initState
  