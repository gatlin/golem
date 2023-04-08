{-u
Module : Tubes
Description : Stream processing with a series of tubes
Maintainer : gatlin@niltag.net
Stability : experimental

My interest in stream processing was re-ignited by the excellent project
@https://github.com/iokasimov/pipeline@

Pipeline casts the Pipes / Conduit-style of programming in terms of 'CPS'.

Since I have my own special 'CPS' I have plagiarized Pipeline's approach.
My goal is to determine how this relates to 'Orc' and then have one module
subsume the other (or learn a valuable reason for why not).
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module Tubes
  ( -- * A series of tubes
    -- ** Construction and evaluation
    Series(..)
  , deliver
  , yield
  , await
  , finish
  , embed
    -- ** Combination
  , (><)
    -- * Utility
  , Tube(..)
  , Source(..)
  , Sink(..)
  , pause
  , suspend )
where

import CPS (CPS(..))

-- | The head of a stream processing series.
newtype Source i t r = Source { play  :: Sink i t r -> t r }

-- | The reservoir at the end of a stream processing pipeline.
newtype Sink   o t r = Sink   { resume :: o -> Source o t r -> t r }

-- | An intermediate link in the series of tubes.
newtype Tube i o r t a = Tube { flow :: Source i t r -> Sink o t r -> t r }
  deriving (Functor)

-- | A 'Series' of 'Tube's is the (delimited) continuation embedding into some
-- base type @t :: * -> *@.
-- 'Series' may be connected into, well, a series via the '(><)' operator.
-- When the series finishes evaluating it will result in a value of type @r@.
type Series i o t a r = CPS r (Tube i o r t) a

-- | A covariant mapping of a 'Source i t r' into 'Source o t r'.
pause :: (() -> Tube i o r t a) -> Source i t r -> Source o t r
pause next ik = Source $ \ok -> (flow $ next ()) ik ok

-- | A contravariant mapping of a 'Sink o t r' into a 'Sink i t r'.
suspend :: (i -> Tube i o r t a) -> Sink o t r -> Sink i t r
suspend next ok = Sink $ \v ik -> flow (next v) ik ok

-- | Blocking-wait for an upstream value from the 'Series'.
await :: Series i o t i r
await = CPS $ \k -> Tube $ \ik ok -> play ik (suspend k ok)

-- | Yield a value downstream in the 'Series'.
yield :: o -> Series i o t () r
yield v = CPS $ \k -> Tube $ \ik ok -> resume ok v (pause k ik)

-- | A 'Series' which does nothing.
finish :: Monad t => Series i o t () ()
finish = CPS $ \_ -> Tube $ \_ _ -> pure ()

-- | Embeds a value with side effects into an appropriate 'Series'.
embed :: Monad t => t a -> Series i o t a ()
embed e = CPS $ \next -> Tube $ \ik ok -> e >>= \v -> flow (next v) ik ok

-- | Constructs a 'Series' of 'Tube's.
(><)
  :: forall i e a o t. Monad t
  => Series i e t () ()
  -> Series e o t () ()
  -> Series i o t a ()
p >< q = CPS $ \k -> Tube $ \ik ok ->
  flow (q # end) (pause (\() -> p # end) ik) ok where

  end :: b -> Tube c d () t ()
  end _ = Tube $ \_ _ -> pure ()

-- | "...deliver[s] vast amounts of information" from a 'Series' of tubes. :)
deliver
  :: Monad t
  => Series i o t r r
  -> t r
deliver p = flow (p # (\r -> Tube $ \_ _ -> pure r)) i o where
  i :: Source i t r
  i = Source $ \o -> play i o

  o :: Sink o t r
  o = Sink $ \v i -> resume o v i
