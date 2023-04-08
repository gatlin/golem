{-# LANGUAGE TypeFamilies #-}

module Conway where

import Prelude hiding (lookup)
import Lists (Counted(..))
import Sheet (Coordinate, Sheet2, ISheet2, Ref(..), getRef, Nat2, Indexed(..))
import Control.Arrow ((***))
import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store (StoreT(..), Store, store, experiment)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Rep (Representable(..))

ish2D :: ISheet2 (Int, Int)
ish2D = tabulate (\(col ::: row ::: _) -> (getRef row, getRef col))

sh2D :: Sheet2 (Int, Int)
sh2D = tabulate (\(col ::: row ::: _) -> (getRef row, getRef col))

type Grid = Store Sheet2
type Coord = Coordinate Nat2

fromPair :: (Int, Int) -> Coord
fromPair ~(x, y) = Abs y ::: Abs x ::: CountedNil
{-# INLINE fromPair #-}

mkGrid :: [Coord] -> Grid Bool
mkGrid xs = store lookup focus where
  focus = fromPair (0, 0)
  {-# INLINE focus #-}
  lookup crd = crd `elem` xs
  {-# INLINE lookup #-}

unGrid :: Grid a -> ISheet2 a
unGrid ~(StoreT ~(Identity sh) crd) = Indexed crd sh
{-# INLINE unGrid #-}

type Rule = Grid Bool -> Bool

neighborCoords :: [Coord]
neighborCoords=
  [ fromPair (x, y)
  | x <- [-1, 0, 1]
  , y <- [-1, 0, 1]
  , (x, y) /= (0, 0) ]

basicRule :: Rule
basicRule g =
  (alive && aliveNbors `elem` [2, 3]) || (not alive && aliveNbors == 3)
  where
    alive = extract g
    {-# INLINE alive #-}
    addCoords :: Coord -> Coord -> Coord
    addCoords (y ::: x ::: _) (y' ::: x' ::: _) =
      (y + y') ::: (x + x') ::: CountedNil
    {-# INLINE addCoords #-}
    neighbors = experiment (\s -> addCoords s <$> neighborCoords) g
    {-# INLINE neighbors #-}
    aliveNbors = length (filter id neighbors)
    {-# INLINE aliveNbors #-}

glider, blinker, beacon :: [Coord]
glider = fromPair <$> [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = fromPair <$> [(0, 0), (1, 0), (2, 0)]
beacon = fromPair <$> [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

at :: [Coord] -> (Int, Int) -> [Coord]
at xs ~(x, y) =
  fmap
  ((fromPair . ((+ x) *** (+ y)))
    . (\ (y' ::: x' ::: _) -> (getRef x', getRef y')))
  xs

start :: Grid Bool
start = mkGrid $
     glider `at` (0, 0)
  ++ beacon `at` (15, 5)
  ++ blinker `at` (16, 4)
