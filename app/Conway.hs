{-# LANGUAGE DeriveFunctor, DeriveFoldable, RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
module Conway where
-- base
import Prelude hiding (drop, head, repeat, take, zipWith, lines)
import qualified Prelude as P
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad (liftM2)
import qualified Data.Foldable as F
-- comonad
import Control.Comonad ( Comonad(..), ComonadApply(..), (=>>))

-- = Streams

-- | The following 'Stream' type is inlined from
--
--   https://hackage.haskell.org/package/Stream-0.4.7.2/docs/Data-Stream.html
data Stream a = Cons !a (Stream a) deriving (Eq, Ord, Show, Functor, Foldable)

infixr 5 `Cons`

(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons

fromList :: [a] -> Stream a
fromList = F.foldr Cons (error "Stream.fromList applied to finite list")

toList :: Stream a -> [a]
toList = F.toList

head :: Stream a -> a
head ~(Cons !x _ ) = x
{-# INLINE head #-}

tail :: Stream a -> Stream a
tail ~(Cons _ xs) = xs
{-# INLINE tail #-}

repeat :: a -> Stream a
repeat !x = x `seq` Cons x (repeat x)

take :: Int -> Stream a  -> [a]
take n  = P.take n . toList

drop :: Int -> Stream a -> Stream a
drop n (Cons x xs)
  | n == 0 = x <:> xs
  | n > 0  = drop (n - 1) xs
  | otherwise = error "Stream.drop: negative argument."

unfold :: (c -> (a,c)) -> c -> Stream a
unfold f c =
  let ~(!x,!d) = f c
      xs     = unfold f d
  in x <:> xs

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f ~(Cons x xs) ~(Cons y ys) = Cons (f x y) (zipWith f xs ys)
{-# INLINE zipWith #-}

instance Applicative Stream where
  pure = repeat
  (<*>) = zipWith ($!)

instance Comonad Stream where
  extract = head
  duplicate ~(Cons x xs) = x <:> xs <:> duplicate xs
  {-# INLINE duplicate #-}

instance ComonadApply Stream where
  (<@>) = (<*>)

-- = Tapes

-- | A one-dimensional Stream zipper
data Tape a = Tape
  { _lhs ::  Stream a -- ^ Values to the left of the focus
  , _focus  ::  a
  , _rhs ::  Stream a -- ^ Values to the right of the focus
  } deriving (Functor, Show)

-- | Extract a finite portion of a 'Tape', with the focus on the left.
tapeView :: Int -> Tape a -> [a]
tapeView n ~(Tape _ x rs) = x : take (n - 1) rs

-- | Move a 'Tape' focus to the left.
tapeL :: Tape a -> Tape a
tapeL ~(Tape ~(Cons l ls) c rs) = Tape ls l (Cons c rs)
{-# INLINE tapeL #-}

-- | Move a 'Tape' focus to the right.
tapeR :: Tape a -> Tape a
tapeR ~(Tape ls c ~(Cons r rs)) = Tape (Cons c ls) r rs
{-# INLINE tapeR #-}

-- | Takes a seed value and production rules to produce a 'Tape'
generate
  :: (c -> (a, c)) -- ^ Left-hand element generation rule
  -> (c -> a)      -- ^ Rule for generating the initial focus
  -> (c -> (a, c)) -- ^ Right-hand element generation rule
  -> c             -- ^ The initial seed
  -> Tape a
generate prev center next =
  Tape <$> unfold prev <*> center <*> unfold next
{-# INLINE generate #-}

-- | Transform the branches and focus of a 'Tape'.
shift
  :: (a -> a) -- ^ Rule to shift the left-hand
  -> (a -> a) -- ^ Rule to shift the right-hand
  -> a        -- ^ New focus value
  -> Tape a
shift prev next =
  generate (dup . prev) id (dup . next)
  where dup !a = (a, a)

(&&&) :: (t -> a) -> (t -> b) -> t -> (a, b)
f &&& g = \v -> (f v, g v)

tapeFromPattern :: [a] -> Tape a
tapeFromPattern ptn = generate (go pred) (ptn !!) (go succ) 0
  where go k n = ((ptn !!) &&& id) (k n `mod` length ptn)

-- | A 'Tape' is 'Applicative'.
instance Applicative Tape where
  pure = Tape <$> pure <*> id <*> pure

  ~(Tape fls fc frs) <*> ~(Tape ls c rs) =
    Tape (fls <*> ls) (fc c) (frs <*> rs)

-- | A 'Tape' is also a 'Comonad'.
instance Comonad Tape where
  extract   = _focus
  {-# INLINE extract #-}
  duplicate = shift tapeL tapeR
  {-# INLINE duplicate #-}

instance ComonadApply Tape where
  (<@>) = (<*>)

-- = Interlude: Two-dimensional structures
{- | law:
1. up . down = down . up = id = left . right = right . left -}
class TwoD t where
  up :: t a -> t a
  down :: t a -> t a
  left :: t a -> t a
  right :: t a -> t a

  upBy, downBy, leftBy, rightBy :: Int -> t a -> t a
  upBy n td
    | n == 0 = td
    | n > 0  = upBy (n-1) (up td)
    | otherwise = downBy (-1*n) td

  downBy n td
    | n == 0 = td
    | n > 0  = downBy (n-1) (down td)
    | otherwise = upBy (-1*n) td

  leftBy n td
    | n == 0 = td
    | n > 0  = leftBy (n-1) (left td)
    | otherwise = rightBy (-1*n) td

  rightBy n td
    | n == 0 = td
    | n > 0  = rightBy (n-1) (right td)
    | otherwise = leftBy (-1*n) td

-- = Sheets

-- | A two-dimensional 'Tape'.
-- Up and down are left and right on the outer tape, respectively.
newtype Sheet a = Sheet (Tape (Tape a))
  deriving (Functor, Show)

-- | Generalization of 'shift'
-- | Extract a finite subset of a 'Sheet' focused on some point.
sheetView :: Int -> Int -> Sheet a -> [[a]]
sheetView rows cols (Sheet sh) = sh `seq` sh <&> tapeView cols & tapeView rows

makeSheet :: a -> [[a]] -> Sheet a
makeSheet background rows = Sheet $! Tape (pure fz) r (fromList rs) where
  (!r:rs) = (map line rows) ++ (toList (pure fz)) -- [Tape a]
  !ds = pure background -- Stream a
  !dl = toList ds -- [a]
  !fz = pure background -- Tape a
  line (!c:cs) = Tape ds c (fromList (cs ++ dl)) -- [a] -> Tape a

instance Comonad Sheet where
  extract (Sheet s) = extract $ extract s
  {-# INLINE extract #-}
  duplicate = Sheet . fmap horizontal . vertical where
    horizontal, vertical :: Sheet a -> Tape (Sheet a)
    horizontal = shift left right
    vertical = shift up down
  {-# INLINE duplicate #-}

instance ComonadApply Sheet where
  (Sheet f) <@> (Sheet x) = Sheet ((<@>) <$> f <@> x)

instance Applicative Sheet where
  (<*>) = (<@>)
  pure v = Sheet (pure (pure v))

instance TwoD Sheet where
  up (Sheet !t) = Sheet (t & tapeL)
  {-# INLINE up #-}
  down (Sheet !t) = Sheet (t & tapeR)
  {-# INLINE down #-}
  left (Sheet !tt) = Sheet (tt <&> tapeL)
  {-# INLINE left #-}
  right (Sheet !tt) = Sheet (tt <&> tapeR)
  {-# INLINE right #-}

-- = Cellular Automata code

data Cell = X | O deriving (Eq, Show)
type Pattern = Sheet Cell

cardinality :: [ Cell ] -> Int
cardinality = length . filter (== X)

-- | Extract the neighbors of a 'Sheet' focus (a sub-'Sheet')
neighbors :: TwoD t => [t a -> t a]
neighbors = [ left, right, up, down, left . up, left . down, right . up, right . down ]
{-# INLINE neighbors #-}

aliveNeighbors2D :: Sheet Cell -> Int
aliveNeighbors2D z = neighbors <&> fn & cardinality where
  fn dir = extract $! dir z
  {-# INLINE fn #-}

conway2D :: Sheet Cell -> Cell
conway2D z = case aliveNeighbors2D z of
  2 -> extract z
  3 -> X
  _ -> O

animate :: [[Cell]] -> Stream Pattern
animate ptn = unfold fn sh where
  sh :: Sheet Cell
  sh = makeSheet O ptn
  fn !g =
    let !g' = g =>> conway2D
    in  (g, g')
