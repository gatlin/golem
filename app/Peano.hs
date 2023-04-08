{-|
Module : Peano
Description : Type-level natural numbers, ordering, and arithmetic.
Maintainer : gatlin@niltag.net

Definition of type-level Peano numerals along with type operators to inspect and
manipulate them.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Peano
  ( -- * Natural numbers
    Natural(..)
  , Z
  , S
  , ReifyNatural(..)
    -- * Partial ordering
    -- ** Less-than
  , type (<)
  , LT(..)
    -- ** Less-than-or-equal
  , type (<=)
  , LTE(..)
    -- * Arithmetic
    -- ** Addition
  , Plus(..)
    -- ** Subtraction
  , Minus(..)
  )
where

import Data.Kind (Type)

-- * Naturals

data Z
data S (n :: k)

-- | Natural numbers paired with type-level natural numbers.
-- These terms each act as a witness for a particular natural.
data Natural (n :: Type)
  = (n ~ Z) => Zero
  | forall t. (n ~ S t) => Succ (Natural t)

-- | Given a context expecting a particular natural,
-- we can manufacture it from the ether.
class ReifyNatural (n :: Type) where
  reifyNatural :: Natural n

instance ReifyNatural Z where
  reifyNatural = Zero

instance (ReifyNatural n) => ReifyNatural (S n) where
  reifyNatural = Succ (reifyNatural :: Natural n)

-- | Type-level <= test.
class LTE (a :: k1) (b :: k2) where
  type LessThanOrEqual a b :: Bool

instance LTE Z Z where
  type LessThanOrEqual Z Z = 'True

instance LTE Z (S m) where
  type LessThanOrEqual Z (S m) = 'True

instance LTE (S n) (S m) where
  type LessThanOrEqual (S n) (S m) = LessThanOrEqual n m

instance LTE (S n) Z where
  type LessThanOrEqual (S n) Z = 'False

-- | Type-level < test.
class LT (a :: k1) (b :: k2) where
  type LessThan a b :: Bool

instance LT n m => LT (S n) (S m) where
  type LessThan (S n) (S m) = LessThan n m

instance LT Z (S m) where
  type LessThan Z (S m) = 'True

instance LT (S n) Z where
  type LessThan (S n) Z = 'False

instance LT Z Z where
    type LessThan Z Z = 'False

-- * Peano arithmetic

-- | Natural addition
class Plus (x :: Type) (y :: Type) where
  type x + y :: Type
  plus :: Natural x -> Natural y -> Natural (x + y)

instance Plus x Z where
  type x + Z = x
  plus x Zero = x

instance Plus x y => Plus x (S y) where
  type x + (S y) = S (x + y)
  plus x (Succ y) = Succ (plus x y)

-- | Natural subtraction
class Minus (x :: Type) (y :: Type) where
  type x - y :: Type
  minus :: ( LessThanOrEqual y x ~ 'True )
        => Natural x
        -> Natural y
        -> Natural (x - y)

instance Minus x Z where
  type x - Z = x
  minus x Zero = x

instance (Minus x y) => Minus (S x) (S y) where
  type (S x) - (S y) = x - y
  minus (Succ x) (Succ y) = minus x y

-- | This constraint is a more succinct way of requiring that
-- @a@ be less than or equal to @b@.
type a <= b = (LessThanOrEqual a b ~ 'True, Minus b a)

-- | This constraint is similar to @<=@.
type a < b = (LessThan a b ~ 'True)

