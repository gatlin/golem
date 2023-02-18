{-# LANGUAGE DeriveFunctor, RankNTypes, ExistentialQuantification #-}
module Conway where
-- base
import Prelude hiding (drop, head, repeat, take, zipWith, lines)
import qualified Prelude as P
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad (liftM2)
-- comonad
import Control.Comonad ( Comonad(..), ComonadApply(..), (=>>))

-- * Streams

-- | The following 'Stream' type is inlined from
--
--   https://hackage.haskell.org/package/Stream-0.4.7.2/docs/Data-Stream.html
data Stream a = Cons a (Stream a) deriving (Eq, Ord, Show)

infixr 5 `Cons`

instance Functor Stream where
  fmap f ~(Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Stream where
  pure = repeat
  (<*>) = zipWith ($)

instance Comonad Stream where
  extract = head
  duplicate s@(Cons _ xs) = Cons s (duplicate xs)

instance ComonadApply Stream where
  (<@>) = (<*>)

(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons

fromList :: [a] -> Stream a
fromList (x:xs) = Cons x (fromList xs)
fromList []     = error "Stream.fromList applied to finite list"

head :: Stream a -> a
head (Cons x _ ) = x

repeat :: a -> Stream a
repeat x = Cons x (repeat x)

take :: Int -> Stream a  -> [a]
take n ~(Cons x xs)
  | n == 0    = []
  | n > 0     =  x : (take (n - 1) xs)
  | otherwise = error "Stream.take: negative argument."

drop :: Int -> Stream a -> Stream a
drop n ~(Cons x xs)
  | n == 0 = Cons x xs
  | n > 0  = drop (n - 1) xs
  | otherwise = error "Stream.drop: negative argument."

toList :: Stream a -> [a]
toList (Cons x xs) = x : toList xs

unfold :: (c -> (a,c)) -> c -> Stream a
unfold f c =
  let (x,d) = f c
  in Cons x (unfold f d)

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f ~(Cons x xs) ~(Cons y ys) = Cons (f x y) (zipWith f xs ys)

-- * Tapes

-- | A one-dimensional Stream zipper
data Tape a = Tape
  { _lhs ::  Stream a -- ^ Values to the left of the focus
  , _focus  ::  a
  , _rhs ::  Stream a -- ^ Values to the right of the focus
  } deriving (Functor, Show)

-- | A 'Tape' is 'Applicative'.
instance Applicative Tape where
  pure = Tape <$> pure <*> id <*> pure

  (Tape fls fc frs) <*> (Tape ls c rs) =
    Tape (fls <*> ls) (fc c) (frs <*> rs)

-- | A 'Tape' is also a 'Comonad'.
instance Comonad Tape where
  extract   = _focus
  duplicate = shift tapeL tapeR

instance ComonadApply Tape where
  (<@>) = (<*>)

-- | Extract a finite portion of a 'Tape', with the focus on the left.
tapeView :: Int -> Tape a -> [a]
tapeView 0 _ = []
tapeView n (Tape _ x rs) = [x] ++ take (n - 1) rs

-- | Move a 'Tape' focus to the left.
tapeL :: Tape a -> Tape a
tapeL (Tape (Cons l ls) c rs) = Tape ls l (c <:> rs)

-- | Move a 'Tape' focus to the right.
tapeR :: Tape a -> Tape a
tapeR (Tape ls c (Cons r rs)) = Tape (c <:> ls) r rs

-- | Takes a seed value and production rules to produce a 'Tape'
seed
  :: (c -> (a, c)) -- ^ Left-hand element generation rule
  -> (c -> a)      -- ^ Rule for generating the initial focus
  -> (c -> (a, c)) -- ^ Right-hand element generation rule
  -> c             -- ^ The initial seed
  -> Tape a
seed prev center next =
  Tape <$> unfold prev <*> center <*> unfold next

-- | Transform the branches and focus of a 'Tape'.
shift
  :: (a -> a) -- ^ Rule to shift the left-hand
  -> (a -> a) -- ^ Rule to shift the right-hand
  -> a        -- ^ New focus value
  -> Tape a
shift prev next =
  seed (dup . prev) id (dup . next)
  where dup a = (a, a)

tapeFromPattern :: [a] -> Tape a
tapeFromPattern [] = error "Tape.tapeFromPattern: argument must be non-empty."
tapeFromPattern ptn = Tape lhs c rhs where
  Cons _ lhs = fromList (P.concat (P.repeat (reverse ptn)))
  Cons c rhs = fromList (P.concat (P.repeat ptn))

-- * Sheets

-- | A two-dimensional 'Tape'.
-- Up and down are left and right on the outer tape, respectively.
data Sheet a = Sheet (Tape (Tape a))
  deriving (Functor, Show)

instance Comonad Sheet where
  extract (Sheet s) = extract $ extract s
  duplicate = Sheet . fmap horizontal . vertical

instance ComonadApply Sheet where
  (Sheet f) <@> (Sheet a) = Sheet ((<@>) <$> f <@> a)

instance Applicative Sheet where
  (<*>) = (<@>)
  pure v = Sheet (pure (pure v))

-- | Move the focus of a 'Sheet' up
up :: Sheet a -> Sheet a
up (Sheet t) = Sheet (t & tapeL)

-- | Move the focus of a 'Sheet' down
down :: Sheet a -> Sheet a
down (Sheet t) = Sheet (t & tapeR)

-- | Move the focus of a 'Sheet' left
left :: Sheet a -> Sheet a
left (Sheet tt) = Sheet (tt <&> tapeL)

-- | Move the focus of a 'Sheet' right
right :: Sheet a -> Sheet a
right (Sheet tt) = Sheet (tt <&> tapeR)

-- | Generalization of 'shift' for the horizontal dimension.
horizontal :: Sheet a -> Tape (Sheet a)
horizontal = shift left right

-- | Generalization of 'shift' for the vertical dimension.
vertical :: Sheet a -> Tape (Sheet a)
vertical = shift up down

-- | Extract a finite subset of a 'Sheet' focused on some point.
sheetView :: Int -> Int -> Sheet a -> [[a]]
sheetView rows cols (Sheet sh) = sh <&> tapeView cols & tapeView rows

printSheet :: (Foldable f, Show a) => f a -> IO ()
printSheet = mapM_ (putStrLn . show)

makeSheet :: a -> [[a]] -> Sheet a
makeSheet background list = Sheet $ Tape (pure fz) r (fromList rs) where
  (r:rs) = (map line list) ++ (toList (pure fz))
  ds = pure background
  dl = toList ds
  fz = pure background
  line (l:ls) = Tape ds l (fromList (ls ++ dl))

-- * Cellular Automata code

data Cell = X | O deriving (Eq, Show)

cardinality :: [ Cell ] -> Int
cardinality = length . filter (== X)

-- | Extract the neighbors of a 'Sheet' focus (a sub-'Sheet')
neighbors :: [Sheet a -> Sheet a]
neighbors = horiz ++ vert ++ liftM2 (.) horiz vert where
  horiz   = [ left, right ]
  vert    = [ up, down ]

aliveNeighbors :: Sheet Cell -> Int
aliveNeighbors z = neighbors <&> (\dir -> z & dir & extract) & cardinality

conway :: Sheet Cell -> Cell
conway z = case aliveNeighbors z of
  2 -> extract z
  3 -> X
  _ -> O

animate :: Sheet Cell -> Stream (Sheet Cell)
animate = unfold $ \g -> (g, g =>> conway)

