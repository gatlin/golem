{-# LANGUAGE DeriveFunctor, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

module UI
  (
  -- UI
    UI
  , runUI
  , Action(..)
  , nil
  , mount
  , BehaviorOf
  , behavior
  , Control.Comonad.Cofree.unwrap
  , Screen
  , screen
  -- Drawing utilities
  , glyphCode
  , blockGlyph
  , drawBlock
  , drawRect
  , screenBorder
  , centerText
  , width
  , height
  -- Remainder
  , Run(..)
  , move
  , Dispatcher
  , Interface
  , Component
  , Console(..)
  , instantiate
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Char (ord)
import Control.Comonad (Comonad(..), (=>>))
import Control.Comonad.Cofree (ComonadCofree(unwrap), Cofree, coiter)
import qualified Termbox2 as Tb2

-- = Part 1: Abstract components and spaces.

-- | credit: David Laing calls this 'Pairing'.
class (Functor f, Functor g) => Run f g where
  run :: (a -> b -> r) -> f a -> g b -> r

move :: (Comonad w, Run m w) => w a -> m b -> (b, w a)
move space action = run (,) action (duplicate space)

-- | credit: Ed Kmett calls this 'Co'.
newtype Action space a = Action {
  step :: forall result. space (a -> result) -> result
} deriving Functor

instance (Comonad space) => Applicative (Action space) where
  pure x = Action $ (flip extract) x
  tf <*> tx = tf >>= (flip fmap) tx

instance (Comonad space) => Monad (Action space) where
  action >>= f = Action $ step action . extend (\space a -> step (f a) space)

instance (Functor space) => Run (Action space) space where
  run f action space = step action $ fmap (flip f) space

type Dispatcher base action = base (action ()) -> base ()
type Interface base action view = Dispatcher base action -> view
type Component base space action view = space (Interface base action view)

-- | 'Cofree' does exactly what we want but has an unintuitive name.
type BehaviorOf = Cofree

-- | 'coiter' does exactly what we want but has an unintuitive name.
behavior :: Functor f => (a -> f a) -> a -> BehaviorOf f a
behavior = coiter

-- | Re-exported from @free@ (no name change this time).
unwrap :: ComonadCofree f w => w a -> f (w a)
unwrap = Control.Comonad.Cofree.unwrap

-- | Alias for the universal do-nothing 'Action'.
nil :: Comonad f => Action f ()
nil = return ()

-- = Part 2: Components in the terminal console.

newtype UI a = UI (Tb2.Termbox2 a) deriving ( Functor, Applicative, Monad )

runUI :: UI a -> IO (Either Tb2.Tb2Err a)
runUI (UI tb2) = Tb2.runTermbox2 tb2

-- | A console view
data Console =
  Console
    (UI ())       -- ^ Renders output when called.
    (Tb2.Tb2Event -> IO ()) -- ^ Awaits incoming events.

-- | Legible alias for a common component type.
type Screen m w = Component m w (Action w) Console

-- | Construct a screen component with a given behavior.
screen
  :: Comonad w
  => w a
  -> (a -> UI ())                             -- ^ render
  -> (a -> Tb2.Tb2Event -> IO (Action w ()))  -- ^ update
  -> Screen IO w
screen c r u = c =>> \this emit ->
  let value = extract this
  in  Console (r value) (emit . u value)

-- | Component execution loop.
instantiate
  :: (Comonad w, Run m w)
  => IORef (Component IO w m Console)
  -> UI ()
instantiate ref = UI $ setup >> loop >> Tb2.shutdown where
  setup = Tb2.init >> Tb2.setInputMode Tb2.inputAlt
  loop = do
    space <- liftIO (readIORef ref)
    let Console (UI render) handle = extract space $ \action -> do
          (result, space') <- action >>= return . move space
          writeIORef ref space'
          return result
    Tb2.clear
    render
    Tb2.present
    eventOrNot <- Tb2.peekEvent 10
    case eventOrNot of
      Nothing -> loop
      Just evt -> unless (Tb2._key evt == Tb2.keyCtrlC) $ do
        liftIO (handle evt)
        loop

-- | Sets up a component for execution and catches exceptions.
mount :: (Comonad w, Run m w) => Component IO w m Console -> IO ()
mount component = do
  ret <- newIORef component >>= runUI . instantiate
  case ret of
    Left err -> putStrLn $ concat [ "Error: ", show err ]
    Right _ -> return ()

-- = Part 3: Drawing utilities.

glyphCode :: Integral n => Char -> n
glyphCode = fromIntegral . ord

blockGlyph :: Integral n => n
blockGlyph = glyphCode '▄'

drawBlock :: Int -> Int -> UI ()
drawBlock x y = UI $ Tb2.setCell x y blockGlyph Tb2.colorWhite Tb2.colorDefault

drawRect :: Int -> Int -> Int -> Int -> UI ()
drawRect left top w h = UI $ do
  let bottom = top+h-1
  let right = left+w-1
  let setCell x y ch = Tb2.setCell x y ch Tb2.colorWhite Tb2.colorDefault
  forM_ [left..right] $ \i -> do
    setCell i top 0x2500
    setCell i bottom 0x2500
  forM_ [top..bottom] $ \i -> do
    setCell left i 0x2502
    setCell right i 0x2502
  setCell left top 0x250C
  setCell right top 0x2510
  setCell left bottom 0x2514
  setCell right bottom 0x2518

screenBorder :: Int -> UI ()
screenBorder border = do
  w <- width
  h <- height
  drawRect border border (w-2*border) (h-2*border)

centerText :: String -> UI ()
centerText msg = UI $ do
  w <- Tb2.width
  h <- Tb2.height
  let cx = ((w `div` 2) - ((length msg) `div` 2))
  let cy = h `div` 2
  let fgAttrs = Tb2.colorGreen <> Tb2.attrUnderline <> Tb2.attrBold
  let bgAttrs = Tb2.colorMagenta
  Tb2.print cx cy fgAttrs bgAttrs msg

width, height :: UI Int
width = UI Tb2.width
height = UI Tb2.height
