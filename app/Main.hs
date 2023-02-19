{-# LANGUAGE DeriveFunctor #-}
module Main (main) where

import System.Environment (getArgs)
import System.IO (readFile)
import Control.Monad (forM_, when)
import Control.Comonad (Comonad(extract))
import Termbox2 (Tb2Event(_ch))
import Conway (Cell(X), Cell(O))
import qualified Conway as C
import UI
  ( UI
  , (<->)
  , Action(..)
  , nil
  , BehaviorOf
  , Day(..)
  , behavior
  , unwrap
  , screen
  , Screen
  )
import qualified UI

data CounterApi k = Counter { _tick :: k } deriving (Functor)
type Counter = BehaviorOf CounterApi

counter :: Int -> Counter Int
counter = behavior $ \n -> Counter (n+1)

-- | Our application consists of a stream of frames to render and a counter to
-- keep track of them.
type App = Day C.Stream Counter

-- | Advance the stream to the next frame.
next :: Action App ()
next = Action $ \(Day f s c) -> extract (Day f (C.drop 1 s) c) ()

-- | Increment the frame counter.
tick :: Action App ()
tick = Action $ \(Day f s c) ->
  extract (Day f s (_tick (unwrap c))) ()

-- | A terminal UI component with behavior defined by a 'C.Stream C.Pattern'.
-- Advances a Conway Game of Life cellular automaton one step for each press of
-- the space bar.
app :: C.Pattern -> Screen App IO
app start = screen ((C.animate start) <-> (counter 1)) render update where

  render :: (C.Pattern, Int) -> UI ()
  render (pattern, frameNumber) = do
    UI.screenBorder 0
    UI.statusText $ concat [ "Frame: ", show frameNumber ]
    w <- UI.width
    h <- UI.height
    let rows = C.sheetView (h-2) (w-2) pattern
    forM_ (zip [1..] rows) $ \(y, row) ->
      forM_ (zip [1..] row) $ \(x, cell) ->
        when (cell == X) $ UI.drawBlock x y

  update :: (C.Pattern, Int) -> Tb2Event -> IO (Action App ())
  update _ et = return $
    if (_ch et == UI.glyphCode ' ')
      then (tick >> next)
      else nil

-- | Load a 'C.Pattern' from a given file path (or exit gracelessly).
patternFromFile :: String -> IO C.Pattern
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
