{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Padelude (module P, module Set, (++), Nat) where

import           Data.Set.Monad as Set (Set)
import           Protolude      as P hiding (Nat, Set, on, (++))

infixr 5  ++
(++) :: Monoid a => a -> a -> a
(++) = mappend

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Ord, Show)

instance Enum Nat where
    fromEnum Zero     = 0
    fromEnum (Succ n) = 1 + fromEnum n

    toEnum 0 = Zero
    toEnum x = Succ (toEnum (x - 1))

instance Monoid Nat where
    mempty = Zero
    mappend Zero x     = x
    mappend (Succ x) y = mappend x (Succ y)
