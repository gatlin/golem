module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile)
import Control.Monad (forM_, when)
import Control.Comonad (Comonad(extract))
import Termbox2 (Termbox2, Tb2Event(_ch), width, height)
import Conway (Cell(X), Cell(O))
import qualified Conway as C
import qualified UI

type Frame = C.Sheet Cell

next :: UI.Action C.Stream ()
next = UI.Action $ \frames -> extract (C.drop 1 frames) ()

app :: Frame -> UI.Screen IO C.Stream
app start = UI.screen (C.animate start) render update where

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

frameFromFile :: String -> IO Frame
frameFromFile path = do
  contents <- readFile path
  let fr = fmap (fmap xform) (lines contents)
  return $ C.makeSheet O fr

  where
    xform ch | ch == 'X' = X
             | ch == 'O' = O
             | otherwise = error "invalid golem file."

main :: IO ()
main = do
  args <- getArgs
  when (0 == length args) (error "please provide a golem file.")
  frame <- frameFromFile (args !! 0)
  UI.mount (app frame)
