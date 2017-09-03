{-# LANGUAGE ScopedTypeVariables #-}
module Util.Vector (vectorSort) where

import           Padelude

import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as VA

vectorSort :: Ord a => V.Vector a -> V.Vector a
vectorSort initial
  = V.create $
      do
        vec <- V.thaw initial
        VA.sort vec
        return vec
