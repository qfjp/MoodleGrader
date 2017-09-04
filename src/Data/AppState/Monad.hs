{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.AppState.Monad
    ( module AppState
    , AppState
    , appState
    , appState'
    , empty
    , getBlist
    , getMarks
    , singleton
    , union
    , unions
    )
    where

import           Padelude            hiding (empty, show)
import qualified Prelude             as Pre (error)

import           Brick.Widgets.List  as L
import qualified Control.Applicative as A
import           Control.Lens        hiding (sets)
import qualified Data.Foldable       as Foldable
import qualified Data.Functor        as F
import qualified Data.Set.Monad      as S
import qualified Data.Vector         as V
import           GHC.Read
import           Text.Show

import           Data.AppState       as AppState (BList)
import qualified Data.AppState       as AP

data AppState n a where
  Prim   :: (Ord a) => AP.AppState n a -> AppState n a
  Return :: a -> AppState n a
  Bind   :: AppState n a -> (a -> AppState n b) -> AppState n b
  Zero   :: AppState n a
  Plus   :: AppState n a -> AppState n a -> AppState n a

run :: forall a n. (Monoid n, Ord a) => AppState n a -> AP.AppState n a
run (Prim s)
  = s
run (Return a)
  = AP.singleton mempty a
run Zero
  = AP.empty
run (Plus ma mb)
  = run ma `AP.union` run mb
run (Bind (Prim s) f)
  = AP.appState (origName ++ names1 ++ names2) lists sets
  where
    origName = AP._blist s ^. L.listNameL
    (sets, names1) = (apSet, names)
      where
        setsAsList = S.toList (AP._marked s)
        applied = AP.unions $ map (run . f) $ setsAsList
        names = AP._blist applied ^. L.listNameL
        apLst = AP._blist applied ^. L.listElementsL
        apSet = (S.fromList . V.toList) apLst ++ AP._marked applied
    (lists, names2) = (apLst, names)
      where
        vecsAsList = V.toList ((AP._blist s) ^. L.listElementsL)
        applied = AP.unions $ map (run . f) vecsAsList
        names = AP._blist applied ^. L.listNameL
        apSet = AP._marked applied
        apLst =  (V.fromList . S.toList) apSet
              ++ AP._blist applied ^. L.listElementsL

run (Bind (Return a) f)             = run (f a)
run (Bind Zero _)                   = AP.empty
run (Bind (Plus (Prim s) ma) f)     = run (Bind (Prim (s `AP.union` run ma)) f)
run (Bind (Plus ma (Prim s)) f)     = run (Bind (Prim (run ma `AP.union` s)) f)
run (Bind (Plus (Return a) ma) f)   = run (Plus (f a) (Bind ma f))
run (Bind (Plus ma (Return a)) f)   = run (Plus (Bind ma f) (f a))
run (Bind (Plus Zero ma) f)         = run (Bind ma f)
run (Bind (Plus ma Zero) f)         = run (Bind ma f)
run (Bind (Plus (Plus ma mb) mc) f) = run (Bind (Plus ma (Plus mb mc)) f)
run (Bind (Plus ma mb) f)           = run (Plus (Bind ma f) (Bind mb f))
run (Bind (Bind ma f) g)            = run (Bind ma (\a -> Bind (f a) g))

empty :: (Monoid n, Ord a) => AppState n a
empty = Prim AP.empty

singleton :: (Ord a) => n -> a -> AppState n a
singleton = (Prim .) . AP.singleton

union :: (Monoid n, Ord a) => AppState n a -> AppState n a -> AppState n a
union = ((flip (.)) run) . (((Prim .) . AP.union) . run)

unions :: (Monoid n, Ord a) => [AppState n a] -> AppState n a
unions = Prim . AP.unions . map run

appState :: (Ord a) => n -> V.Vector a -> Set a -> AppState n a
appState = ((Prim .) .) . AP.appState

appState' :: (Ord a) => BList n a -> Set a -> AppState n a
appState' = (Prim .) . AP.appState'

getBlist :: (Monoid n, Ord a) => AppState n a -> BList n a
getBlist = AP._blist . run

getMarks :: (Monoid n, Ord a) => AppState n a -> Set a
getMarks = AP._marked . run

instance F.Functor (AppState n) where
  fmap = liftM

instance A.Applicative (AppState n) where
  pure  = return
  (<*>) = ap

instance A.Alternative (AppState n) where
  empty = Zero
  (<|>) = Plus

instance Monad (AppState n) where
  return = Return
  (>>=)  = Bind

instance MonadPlus (AppState n) where
  mzero = Zero
  mplus = Plus

instance (Monoid n, Ord a) => Monoid (AppState n a) where
  mempty  = empty
  mappend = union
  mconcat = unions

instance Foldable (AppState n) where
    foldr f def m
      = case m of
          Prim s -> AP.foldr f def s
          Return a -> f a def
          Zero -> def
          Plus ma mb -> Foldable.foldr f (Foldable.foldr f def ma) mb
          Bind s g -> Foldable.foldr f' def s
            where f' x b = Foldable.foldr f b (g x)

instance Monoid n => Traversable (AppState n) where
    --traverse :: Applicative f => (a -> f b) -> AppState n a -> f (AppState n b)
    traverse f m
      = case m of
          Prim s     -> Pre.error "undefined"
          Return a   -> Return <$> f a
          Zero       -> pure Zero
          Plus ma mb -> Pre.error "undefined"
          Bind s g   -> Pre.error "undefined"

instance (Eq n, Monoid n, Ord a) => Eq (AppState n a) where
  s1 == s2 = run s1 == run s2

instance (Monoid n, Eq n, Ord n, Ord a) => Ord (AppState n a) where
  compare s1 s2 = compare (run s1) (run s2)

instance (Monoid n, Show n, Show a, Ord a) => Show (AppState n a) where
  show = show . run
