{-# LANGUAGE DeriveFunctor, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, StrictData #-}
{-# LANGUAGE ExistentialQuantification, BangPatterns, TypeFamilies #-}
{-# LANGUAGE ImplicitParams, ConstraintKinds #-}

module UI
  (
  -- UI
    UI
  , Action(..)
  , mount
  , BehaviorOf
  , behavior
  , Control.Comonad.Cofree.unwrap
  , Activity
  , modify
  , put
  , get
  , Event(..)
  -- Drawing utilities
  , glyphCode
  , blockGlyph
  , drawBlock
  , drawRect
  , screenBorder
  , centerText
  , statusText
  , width
  , height
  -- Remainder
  , Callback
  , Interface
  , Component
  , Console(..)
  , move
  , hoist
  -- Re-exports for convenience
  , Control.Comonad.Store.Store
  , Control.Comonad.Store.store
  , Control.Comonad.Store.runStore
  , Control.Monad.IO.Class.liftIO
  ) where

import Control.Exception (Exception(..), bracket_, throwIO)
import Control.Monad (forM_, forever)
import Data.Char (ord)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Control.Comonad (Comonad(..), (=>>))
import Control.Comonad.Cofree (ComonadCofree(unwrap), Cofree, coiter)
import Control.Comonad.Store (ComonadStore(..), Store, store, runStore)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Termbox2 as Tb2
import Tubes ((><), await, deliver, embed, yield)

-- Part 1: Components, actions, and spaces.

type Callback effect action = action effect () -> effect ()
type Interface effect action view = Callback effect action -> view
type Component effect space action view = space (Interface effect action view)

-- | Represents some action performed with or on a given component @space@.
-- These actions have side effects in a base monad.
newtype Action space effect a = Action {
  work :: forall r. space (a -> effect r) -> effect r
} deriving (Functor)

instance Comonad space => Applicative (Action space effect) where
  pure a = Action (`extract` a)
  mf <*> ma = mf >>= \f -> fmap f ma

instance Comonad space => Monad (Action space effect) where
  Action k >>= f = Action (k . extend (\wa a -> work (f a) wa))

instance Comonad space => MonadTrans (Action space) where
  lift m = Action (extract . fmap (m >>=))

instance (Comonad space, MonadIO effect) => MonadIO (Action space effect) where
  liftIO = lift . liftIO

-- | Carries out an 'Action' in a space yielding a result with side effects.
move
  :: (Functor space)
  => (a -> b -> effect r)
  -> Action space effect a
  -> space b
  -> effect r
move f a s = work a (fmap (flip f) s)

-- | Hoist an 'Action' for one space into a different space contravariantly.
hoist
  :: (forall x. w x -> v x)
  -> Action v effect a
  -> Action w effect a
hoist transform action = Action $ work action . transform

-- | 'Action' for components built from a 'ComonadStore': modifies state.
modify :: (ComonadStore s w) => (s -> s) -> Action w effect ()
modify fn = Action $ \st -> extract (seeks fn st) ()

-- | 'Action' for components built from a 'ComonadStore': overwrites state.
put :: (ComonadStore s w) => s -> Action w effect ()
put x = Action $ \st -> extract (seek x st) ()

-- | 'Action' for components built from a 'ComonadStore': loads state.
get :: (ComonadStore s w) => Action w effect s
get = Action $ \st -> extract st (pos st)

-- | Defines a space with the behavior of a given base functor.
type BehaviorOf = Cofree

-- | Constructs a space with the behavior of a given base functor.
behavior :: Functor f => (a -> f a) -> a -> BehaviorOf f a
behavior = coiter

-- Part 2: Components in the terminal console.

-- | DSL based on 'Tb2.Termbox2' for UI drawing operations
-- The decisions to not derive 'MonadIO' or export the constructor are
-- deliberate.
newtype UI a = UI (Tb2.Termbox2 a) deriving ( Functor, Applicative, Monad )

-- | FIXME this newtype wrapper is purely due to laziness and a better Event
-- type should be created so the tb2 abstraction does not leak.
newtype Event = Event Tb2.Tb2Event deriving (Show, Eq)

fromTb2Event :: Tb2.Tb2Event -> Maybe Event
fromTb2Event = Just . Event

-- | A console view.
data Console =
  Console
    (Event -> IO ()) -- ^ Awaits incoming events.
    (UI ())          -- ^ Renders output when called.

type Activity w m = Component m w (Action w) Console

data Shutdown = Shutdown deriving Show
instance Exception Shutdown

quit :: MonadIO m => m a
quit = liftIO $ throwIO Shutdown

setup, dispose :: Tb2.Termbox2 ()
setup = Tb2.init
dispose = Tb2.shutdown

loop :: (Comonad space, ?ref :: IORef (Activity space IO)) => Tb2.Termbox2 ()
loop = deliver $ events >< display where
  events = forever $ do
    !mEvent <- embed Tb2.pollEvent
    case mEvent of
      Nothing -> return ()
      Just !event -> if Tb2._key event == Tb2.keyCtrlQ
        then embed quit
        else maybe (return ()) yield $! fromTb2Event event
  display = forever $ do
    space <- embed $ liftIO $! atomicModifyIORef' ?ref $ \sp -> (sp, sp)
    let ~(Console handle ~(UI render)) = extract space $ \action -> do
          space' <- move (const id) action (space =>> return)
          atomicModifyIORef' ?ref $ const (space', ())
    embed $ Tb2.clear >> render >> Tb2.present
    await >>= embed . liftIO . handle

-- | Sets up a component for execution and catches exceptions.
mount :: Comonad space => Activity space IO -> IO ()
mount component = do
  ref <- newIORef component
  let ?ref = ref
  bracket_
    (Tb2.runTermbox2 setup)
    (Tb2.runTermbox2 dispose)
    (Tb2.runTermbox2 loop)

-- Part 3: Drawing utilities.

glyphCode :: Integral n => Char -> n
glyphCode = fromIntegral . ord

blockGlyph :: Integral n => n
blockGlyph = glyphCode '▄'

drawBlock :: Int -> Int -> UI ()
drawBlock x y = UI $! Tb2.setCell x y blockGlyph Tb2.colorWhite Tb2.colorDefault

drawRect :: Int -> Int -> Int -> Int -> UI ()
drawRect left top w h = UI $! do
  let bottom = top+h-1
  let right = left+w-1
  let setCell x y ch = Tb2.setCell x y ch Tb2.colorGreen Tb2.colorBlack
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
  drawRect border border (w-border) (h-border)

centerText :: String -> UI ()
centerText msg = UI $! do
  w <- Tb2.width
  h <- Tb2.height
  let cx = (w `div` 2) - length msg `div` 2
  let cy = h `div` 2
  let fgAttrs = Tb2.colorGreen <> Tb2.attrUnderline <> Tb2.attrBold
  let bgAttrs = Tb2.colorMagenta
  Tb2.print cx cy fgAttrs bgAttrs msg

statusText :: String -> UI ()
statusText msg = UI $! do
  w <- Tb2.width
  h <- Tb2.height
  let cx = w - length msg - 2
  let cy = h - 2
  Tb2.print cx cy Tb2.colorGreen Tb2.colorDefault msg

width, height :: UI Int
width = UI Tb2.width
height = UI Tb2.height

