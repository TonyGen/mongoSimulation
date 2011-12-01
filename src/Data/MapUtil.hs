{-# LANGUAGE TupleSections #-}

module Data.MapUtil where

import Prelude hiding (lookup, map)
import Control.Applicative ((<$>))
import Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Ord (comparing)

upsert :: (Ord k) => (Maybe a -> a) -> k -> Map k a -> Map k a
upsert f = alter (Just . f)

findMinM :: (Ord k) => Map k v -> Maybe (k, v)
-- ^ return min key entry in map. Nothing if empty
findMinM d = if M.null d then Nothing else Just (M.findMin d)

findMaxM :: (Ord k) => Map k v -> Maybe (k, v)
-- ^ return max key entry in map. Nothing if empty
findMaxM d = if M.null d then Nothing else Just (M.findMax d)

findMaxLTE :: (Ord k) => k -> Map k v -> Maybe (k, v)
-- ^ find maximum key <= k. Nothing if no key <= k
findMaxLTE k d = maybe (findMaxM a) (Just . (k,)) mv
	where (a, mv, _) = M.splitLookup k d

diff :: (Ord k) => Map k v -> S.Set k -> Map k v
-- ^ remove keys from map
diff d s = M.difference d $ M.fromAscList $ (,()) <$> S.toAscList s

filterKey :: (Ord k) => (k -> Bool) -> Map k v -> Map k v
filterKey f = filterWithKey (\k _ -> f k)

mapSome :: (Ord k) => (v -> Bool) -> (v -> v) -> Map k v -> Map k v
-- ^ Update values that satisfy predicate
mapSome p u = M.map $ \v -> if p v then u v else v

list :: Map k v -> [(k, v)]
list = toAscList

zipWith :: (Ord k) => (a -> b -> c) -> a -> b -> Map k a -> Map k b -> Map k c
-- ^ Union maps together supply given defaults to function when missing
zipWith f a b da db = unions [dc, dac, dbc] where
	dc = intersectionWith f da db
	dac = map (flip f b) (difference da dc)
	dbc = map (f a) (difference db dc)

groupOn :: (Ord k, Ord a) => (v -> a) -> Map k v -> Map a (Map k v)
-- ^ group by given attribute of values in map
groupOn at = foldrWithKey add empty where
	add k v = alter (Just . insert k v . maybe empty id) (at v)

lt, gt, lte, gte :: (Ord k) => k -> Map k v -> Map k v
lt k = fst . split k
gt k = snd . split k
lte k d = let (d', mv, _) = splitLookup k d in (maybe id (insert k) mv) d'
gte k d = let (_, mv, d') = splitLookup k d in (maybe id (insert k) mv) d'
