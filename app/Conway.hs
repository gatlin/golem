{-# LANGUAGE DeriveFunctor, RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
module Conway where
-- base
import Prelude hiding (drop, head, repeat, take, zipWith, lines)
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad (liftM2)
-- comonad
import Control.Comonad ( Comonad(..), ComonadApply(..), (=>>), kfix)

-- = Streams

-- | The following 'Stream' type is inlined from
--
--   https://hackage.haskell.org/package/Stream-0.4.7.2/docs/Data-Stream.html
data Stream a = Cons !a (Stream a) deriving (Eq, Ord, Show)

infixr 5 `Cons`

(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons
{-# INLINE (<:>) #-}

fromList :: [a] -> Stream a
fromList = foldr (<:>) (error "Stream.fromList applied to finite list")
{-# INLINE fromList #-}

head :: Stream a -> a
head (Cons x _ ) = x
{-# INLINE head #-}

tail :: Stream a -> Stream a
tail (Cons _ xs) = xs
{-# INLINE tail #-}

repeat :: a -> Stream a
repeat x = Cons x (repeat x)
{-# INLINE repeat #-}

take :: Int -> Stream a  -> [a]
take n ~(Cons x xs)
  | n == 0    = []
  | n > 0     =  x : take (n - 1) xs
  | otherwise = error "Stream.take: negative argument."
{-# INLINE take #-}

drop :: Int -> Stream a -> Stream a
drop n ~(Cons x xs)
  | n == 0 = x <:> xs
  | n > 0  = drop (n - 1) xs
  | otherwise = error "Stream.drop: negative argument."

toList :: Stream a -> [a]
toList (Cons x xs) = x : toList xs
{-# INLINE toList #-}

unfold :: (c -> (a,c)) -> c -> Stream a
unfold f c =
  let (x,d) = f c
  in Cons x (unfold f d)

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)
{-# INLINE zipWith #-}

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  {-# INLINE fmap #-}

instance Applicative Stream where
  pure = repeat
  (<*>) = zipWith ($)

instance Comonad Stream where
  extract = head
  duplicate (Cons x xs) = x <:> xs <:> duplicate xs

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
tapeView 0 _ = []
tapeView n (Tape _ x rs) = x : take (n - 1) rs
{-# INLINE tapeView #-}

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

-- | Transform the branches and focus of a 'Tape'.
shift
  :: (a -> a) -- ^ Rule to shift the left-hand
  -> (a -> a) -- ^ Rule to shift the right-hand
  -> a        -- ^ New focus value
  -> Tape a
shift prev next =
  generate (dup . prev) id (dup . next)
  where dup a = (a, a)

(&&&) :: (t -> a) -> (t -> b) -> t -> (a, b)
f &&& g = \v -> (f v, g v)

tapeFromPattern :: [a] -> Tape a
tapeFromPattern ptn = generate (go pred) (ptn !!) (go succ) 0
  where go k n = ((ptn !!) &&& id) (k n `mod` length ptn)

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
sheetView rows cols (Sheet sh) = sh <&> tapeView cols & tapeView rows

makeSheet :: a -> [[a]] -> Sheet a
makeSheet background rows = Sheet $ Tape (pure fz) r (fromList rs) where
  (r:rs) = (map line rows) ++ (toList (pure fz)) -- [Tape a]
  ds = pure background -- Stream a
  dl = toList ds -- [a]
  fz = pure background -- Tape a
  line (c:cs) = Tape ds c (fromList (cs ++ dl)) -- [a] -> Tape a

instance Comonad Sheet where
  extract (Sheet s) = extract $ extract s
  duplicate = Sheet . fmap horizontal . vertical where
    horizontal, vertical :: Sheet a -> Tape (Sheet a)
    horizontal = shift left right
    vertical = shift up down

instance ComonadApply Sheet where
  (Sheet f) <@> (Sheet x) = Sheet ((<@>) <$> f <@> x)

instance Applicative Sheet where
  (<*>) = (<@>)
  pure v = Sheet (pure (pure v))

instance TwoD Sheet where
  up (Sheet t) = Sheet (t & tapeL)
  down (Sheet t) = Sheet (t & tapeR)
  left (Sheet tt) = Sheet (tt <&> tapeL)
  right (Sheet tt) = Sheet (tt <&> tapeR)

-- = Space
newtype Space a = Space (Tape (Tape (Tape a))) deriving (Functor, Show)

backward, forward:: Space a -> Space a
backward (Space t) = Space (t & tapeL)
forward (Space t) = Space (t & tapeR)

toSheet :: Space a -> Sheet a
toSheet (Space ttt) = Sheet $ extract ttt

set :: a -> Space a -> Space a
set value (Space ttt) = Space ttt' where
  Tape ttl ttc ttr = ttt
  Tape tl tc tr = ttc
  Tape l _ r = tc
  ttt' = Tape ttl (Tape tl (Tape l value r) tr) ttr

insert :: [[a]] -> Space a -> Space a
insert rs = insert' rs 0 where
  insert' :: [[a]] -> Int -> Space a -> Space a
  insert' [] h sp = upBy h sp
  insert' (row:rows) h sp = insertRow row 0 sp & down & insert' rows (h+1) where
    insertRow [] w sp' = leftBy w sp'
    insertRow (col:cols) w sp' = set col sp' & right & insertRow cols (w+1)

instance Comonad Space where
  extract (Space s) = extract $ extract $ extract s
  duplicate = Space . fmap (fmap lateral . medial) . radial where
    radial, medial, lateral:: Space a -> Tape (Space a)
    radial = shift backward forward
    medial = shift up down
    lateral = shift left right

instance ComonadApply Space where
  (Space f) <@> (Space x) = Space $ (<@>) <$> ((<$>) (<@>) <$> f) <@> x

instance Applicative Space where
  (<*>) = (<@>)
  pure v = Space $ pure (pure (pure v))

instance TwoD Space where
  left ~(Space ttt) = Space (ttt <&> (<&> tapeL))
  right ~(Space ttt) = Space (ttt <&> (<&> tapeR))
  up ~(Space tt) = Space (tt <&> tapeL)
  down ~(Space tt) = Space (tt <&> tapeR)

-- = Cellular Automata code

data Cell = X | O deriving (Eq, Show)
type Pattern = Sheet Cell

cardinality :: [ Cell ] -> Int
cardinality = length . filter (== X)

-- == Sheets

-- | Extract the neighbors of a 'Sheet' focus (a sub-'Sheet')
neighbors :: TwoD t => [t a -> t a]
neighbors = horiz ++ vert ++ liftM2 (.) horiz vert where
  horiz   = [ left, right ]
  vert    = [ up, down ]

aliveNeighbors2D :: Sheet Cell -> Int
aliveNeighbors2D z =
  neighbors <&> (\dir -> z & dir & extract) & cardinality

conway2D :: Sheet Cell -> Cell
conway2D z = case aliveNeighbors2D z of
  2 -> extract z
  3 -> X
  _ -> O

animate :: [[Cell]] -> Stream (Sheet Cell)
animate ptn = flip unfold (makeSheet O ptn) $ \g -> (g, g =>> conway2D)

-- == Spaces

aliveNeighbors3D :: Space Cell -> Int
aliveNeighbors3D z =
  map (backward .) neighbors <&> (\dir -> z & dir & extract) & cardinality

conway3D :: Space Cell -> Cell
conway3D z = case aliveNeighbors3D z of
  2 -> z & backward & extract
  3 -> X
  _ -> O

ether :: Space (Space Cell -> Cell)
ether = pure conway3D & planeOf (const O) where
  planeOf :: a -> Space a -> Space a
  planeOf value (Space ttt) = Space ttt' where
    Tape ttl _ ttr = ttt
    tc = Tape (pure value) value (pure value)
    ttc = Tape (pure tc) tc (pure tc)
    ttt' = Tape ttl ttc ttr

animate' :: [[Cell]] -> Stream Pattern
animate' config = unfold fn initSpace where
  initSpace = ether & insert (map (map const) config) & kfix
  fn g = (toSheet g, forward g)

