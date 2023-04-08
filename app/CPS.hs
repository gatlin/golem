{-|
Module : CPS
Description : Delimited continuation monad transformer
Maintainer : gatlin@niltag.net
Stability : experimental
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module CPS
  ( -- * Delimited Continuation Monad Transformer
    CPS(..)
  , shift
  , reset
    -- * Utilities
  , lift
  )
  where

import Data.Kind (Type)

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Concurrent.MonadIO hiding (liftIO)

newtype CPS (result :: k) (m :: k -> Type) (answer :: Type) = CPS
  { (#) :: (answer -> m result) -> m result }

reset :: Monad m => CPS r m r -> m r
reset (CPS cc) = cc return

shift :: Monad m => ((a -> m r) -> CPS r m r) -> CPS r m a
shift e = CPS $ \k -> reset (e k)

instance Functor (CPS r m) where
  fmap f c = CPS $ \k -> c # (k . f)

instance Applicative (CPS r m) where
  pure x = CPS ($ x)
  f <*> v = CPS $ \c -> f # ( \g -> v # (c . g) )
  m *> k = m >>= \_ -> k

instance Monad (CPS r m) where
  m >>= k = _join (fmap k m) where
    _join :: CPS r m (CPS r m a) -> CPS r m a
    _join (CPS cc) = CPS (\k -> cc (\(CPS c) -> c k))

instance (HasFork m) => Alternative (CPS () m) where
  empty = CPS $ \_ -> return ()
  p <|> q = CPS $ \k -> do
    fork (p # k)
    q # k
    return ()

instance (HasFork m) => MonadPlus (CPS () m) where
  mzero = empty
  mplus = (<|>)

lift :: Monad m => m a -> CPS r m a
lift v = CPS (v >>=)
