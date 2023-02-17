module Main (main) where

import Control.Monad (forM_, when)
import Control.Comonad (Comonad(extract))
import Termbox2 (Termbox2, Tb2Event(_ch), width, height)
import Conway (Cell(X), Cell(O))
import qualified Conway as C
import qualified UI

type Frame = C.Sheet Cell

next :: UI.Action C.Stream ()
next = UI.Action $ \frames -> extract (C.drop 1 frames) ()

app :: UI.Screen IO C.Stream
app = UI.screen (C.animate glider) render update where

  render :: Frame -> Termbox2 ()
  render frame = do
    UI.screenBorder 0
    w <- width
    h <- height
    let lines = C.sheetView (h-2) (w-2) frame
    forM_ (zip [1..] lines) $ \(y, line) ->
      forM_ (zip [1..] line) $ \(x, cell) ->
        when (cell == X) $ UI.drawBlock x y

  update :: Frame -> Tb2Event -> IO (UI.Action C.Stream ())
  update _ et = return $ if (_ch et == UI.glyphCode ' ') then next else UI.nil

main = UI.mount app

glider, pulsar :: Frame
glider = C.makeSheet O
  [ [ O, X, O ]
  , [ O, O, X ]
  , [ X, X, X ] ]

pulsar = C.makeSheet O
  [ [ O, O, X, X, X, O, O, O, X, X, X, O, O ]
  , [ O, O, O, O, O, O, O, O, O, O, O, O, O ]
  , [ X, O, O, O, O, X, O, X, O, O, O, O, X ]
  , [ X, O, O, O, O, X, O, X, O, O, O, O, X ]
  , [ X, O, O, O, O, X, O, X, O, O, O, O, X ]
  , [ O, O, X, X, X, O, O, O, X, X, X, O, O ]
  , [ O, O, O, O, O, O, O, O, O, O, O, O, O ]
  , [ O, O, X, X, X, O, O, O, X, X, X, O, O ]
  , [ X, O, O, O, O, X, O, X, O, O, O, O, X ]
  , [ X, O, O, O, O, X, O, X, O, O, O, O, X ]
  , [ X, O, O, O, O, X, O, X, O, O, O, O, X ]
  , [ O, O, O, O, O, O, O, O, O, O, O, O, O ]
  , [ O, O, X, X, X, O, O, O, X, X, X, O, O ] ]
