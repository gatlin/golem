module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile)
import Control.Monad (forM_, when)
import Control.Comonad (Comonad(extract))
import Termbox2 (Tb2Event(_ch))
import Conway (Cell(X), Cell(O))
import qualified Conway as C
import qualified UI

-- | Intuitively this is a 2D grid of cells, alive ('X') or dead ('O').
type Pattern = C.Sheet Cell

-- | For any @t@, advances a 'C.Stream t' to its next value.
next :: UI.Action C.Stream ()
next = UI.Action $ \values -> extract (C.drop 1 values) ()

-- | A terminal UI component with behavior defined by a 'C.Stream Pattern'.
-- Advances a Conway Game of Life cellular automaton one step for each press of
-- the space bar.
app :: Pattern -> UI.Screen C.Stream IO
app start = UI.screen (C.animate start) render update where

  render :: Pattern -> UI.UI ()
  render pattern = do
    UI.screenBorder 0
    w <- UI.width
    h <- UI.height
    let rows = C.sheetView (h-2) (w-2) pattern
    forM_ (zip [1..] rows) $ \(y, row) ->
      forM_ (zip [1..] row) $ \(x, cell) ->
        when (cell == X) $ UI.drawBlock x y

  update :: Pattern -> Tb2Event -> IO (UI.Action C.Stream ())
  update _ et = return $ if (_ch et == UI.glyphCode ' ') then next else UI.nil

-- | Load a 'Pattern' from a given file path (or exit gracelessly).
patternFromFile :: String -> IO Pattern
patternFromFile path = do
  contents <- readFile path
  return $ C.makeSheet O $ fmap (fmap xform) (lines contents)
  where
    xform cl | cl == 'X' = X
             | cl == 'O' = O
             | otherwise = error "invalid pattern file."

main :: IO ()
main = do
  args <- getArgs
  when (0 == length args) (error "please provide a pattern file.")
  pattern <- patternFromFile (args !! 0)
  UI.mount (app pattern)
