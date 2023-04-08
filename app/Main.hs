{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Main (main) where

import Control.Comonad (Comonad(..), (=>>))
import Control.Comonad.Store (StoreT(..), runStoreT)
import Control.Monad (forM, forM_, when)
import System.Environment (getArgs)
import qualified UI
import qualified Termbox2 as Tb2
import Sheet ((&))
import qualified Sheet as S
import Conway (Grid, mkGrid, unGrid, Coord, fromPair, Rule, basicRule, start)

--------------------------------------------------------------------------------
-- Conway game!
--------------------------------------------------------------------------------

type App = UI.Store (Grid Bool)

game :: [Coord] -> UI.Activity App IO
game = UI.store render . mkGrid where

  render :: Grid Bool -> UI.Interface IO (UI.Action App) UI.Console
  render this send = UI.Console (send . update) $ do
    !w <- UI.width
    !h <- UI.height
    let !rows = S.take (S.belowBy h & S.rightBy w) $ unGrid this
    forM_ (zip [1..] rows) $ \(row, cols) ->
      forM_ (zip [1..] cols) $ \(col, isAlive) ->
        when isAlive $ UI.drawBlock row col

  update :: UI.Event -> UI.Action App IO ()
  update (UI.Event evt)
    | Tb2._type evt == Tb2.eventKey =
      when (Tb2._ch evt == UI.glyphCode ' ') $ UI.modify (extend basicRule)
    | otherwise = return ()

--------------------------------------------------------------------------------
-- Wrap another component with some runtime stats.
--------------------------------------------------------------------------------

withBorder
  :: Comonad space
  => UI.Activity space effect
  -> UI.Activity (StoreT () space) effect
withBorder inner = StoreT (inner =>> render) () where

  render child () send  =
    let ~(UI.Console uC rC) = extract child $ send . UI.hoist adapt
    in  UI.Console uC $ rC >> UI.screenBorder 0

  adapt wrapped = let (idx, k) = runStoreT wrapped in ($ k) <$> idx
  {-# INLINE adapt #-}

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- | Load a 'C.Pattern' from a given file path (or exit gracelessly).
patternFromFile :: FilePath -> IO [Coord]
patternFromFile path = do
  contents <- readFile path
  coords <- forM (zip (lines contents) [0..]) $ \(row, rowNum) ->
    forM (zip row [0..]) $ \(char, colNum) -> case char of
      'X' -> return [(colNum, rowNum)]
      _   -> return []
  return $ fromPair <$> concat (concat coords)

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ error "Please provide pattern file."
  pattern <- patternFromFile (head args)
  UI.mount $ withBorder $ game pattern
