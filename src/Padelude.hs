{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Padelude (module P, module Set, (++)) where

import           Data.Set.Monad as Set (Set)
import           Protolude      as P hiding (Set, on, (++))

infixr 5  ++
(++) :: Monoid a => a -> a -> a
(++) = mappend
