{-# LANGUAGE DeriveFunctor, TypeOperators #-}

module Main (main) where

import System.Environment (getArgs)
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
  , And(..)
  , behavior
  , unwrap
  , screen
  , Screen
  )
import qualified UI
import Data.Function ((&))

-- Contrived custom behavior example: our app will keep track of the number of
-- times it renders.
--
-- | Defines the /behavior/ (legal operations) supported by the component.
-- In this case, we only define a '_tick' behavior to increment the count by 1.
newtype CounterApi k = Counter { _tick :: k } deriving (Functor)
type Counter = BehaviorOf CounterApi
-- | Concrete implementation of the 'CounterApi'.
counter :: Int -> Counter Int
counter = behavior $ \n -> Counter (n+1)

-- | Our application combines the behaviors of a stream and a 'Counter'.
type App = C.Stream `And` Counter

-- | A terminal UI component ('Screen') with behavior defined by 'App'.
-- Renders the steps of a Game of Life evaluation; displays step count.
app :: [[C.Cell]] -> Screen App IO
app start = screen (C.animate start <-> counter 1) render update where

  render :: (C.Pattern, Int) -> UI ()
  render (pattern, stepNumber) = do
    w <- UI.width
    h <- UI.height
    let rows = pattern & C.sheetView (h-2) (w-2)
    forM_ (zip [1..] rows) $ \(y, row) ->
      forM_ (zip [1..] row) $ \(x, cell) ->
        when (cell == X) $ UI.drawBlock x y
    UI.screenBorder 0
    UI.statusText $ "Step: " ++ show stepNumber

  update :: (C.Pattern, Int) -> Tb2Event -> IO (Action App ())
  update _ et = return $
    if _ch et == UI.glyphCode ' '
      then do
        tick
        nextStep
      else nil

nextStep, tick :: Action App ()
nextStep = Action $ \(And f s c) -> extract (And f (C.drop 1 s) c) ()
tick = Action $ \(And f s c) -> extract (And f s (_tick (unwrap c))) ()

-- | Load a 'C.Pattern' from a given file path (or exit gracelessly).
patternFromFile :: String -> IO [[C.Cell]]
patternFromFile path = do
  contents <- readFile path
  return $ fmap (fmap xform) (lines contents)
  where
    xform cl | cl == 'X' = X
             | cl == 'O' = O
             | otherwise = error "invalid pattern file."

main :: IO ()
main = do
  args <- getArgs
  when (null args) (error "please provide a pattern file.")
  pattern <- patternFromFile (head args)
  UI.mount (app pattern)

