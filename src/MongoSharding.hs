-- | Algorithm simulating MongoDB sharding, particularly its balancing.

{-# LANGUAGE TupleSections, TypeFamilies, DeriveDataTypeable #-}

module MongoSharding (
	emptyDb,
	runM, evalM, execM,
	ChunkData(..), ChunkStat(..), Moving(..),
	get_shards, get_chunks, shardData,
	addShard, removeShard, insertDocument, Document(..),
	balanceChunks, migrateChunks,
	maxChunkSize
) where

import Prelude hiding (null, lookup, filter, map, zipWith)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, second)
import Control.Monad
import Control.MonadUtil
import Control.Monad.State
import Control.Monad.Error
import qualified Data.List as L
import Data.Ord (comparing)
import Data.Map as M
import Data.MapUtil as M
import Data.Maybe
import Data.Record as R

type Sec = Float  -- ^ elapsed time in seconds
type MB = Float  -- ^ megabytes
type MBps = Float  -- ^ megabytes per second
type Key = Integer  -- ^ could have been any ord type. 0 is min key

-- | Sharding config data

type ChunkKey = Key  -- ^ min key of a chunk. identifies a chunk
type ShardNum = Int  -- ^ identifies a shard

data ChunkData = ChunkData {cShard :: ShardNum, cStat :: ChunkStat, cMoving :: Maybe Moving}
	deriving (Show)
set_cShard s d = d {cShard = s}
set_cStat t d = d {cStat = t}
set_cMoving m d = d {cMoving = m}

data ChunkStat = ChunkStat {cSize :: MB, cCount :: Integer, cKeySum :: Key} deriving (Show)
-- ^ cKeySum/cCount ~ median = split point

data Moving = Moving {mvTo :: ShardNum, mvSoFar :: MB} deriving (Show, Eq)
mod_mvSoFar f m = m {mvSoFar = f (mvSoFar m)}

-- | Sharding config database

data Database = Database Record'
instance Record Database where unwrap (Database r) = r; wrap r _ = Database r
emptyDb = Database R.empty

(shards, mod_shards) = field 1 :: Field Database (Map ShardNum Bool)  -- isDraining?
(chunks, mod_chunks) = field 2 :: Field Database (Map ChunkKey ChunkData)
get_shards f = gets (f . shards)
get_chunks f = gets (f . chunks)
modify_shards = modify . mod_shards
modify_chunks = modify . mod_chunks

type M = ErrorT String (State Database)

runM :: M a -> Database -> (Either String a, Database)
runM = runState . runErrorT

execM :: M a -> Database -> Database
execM = execState . runErrorT

evalM :: M a -> Database -> Either String a
evalM = evalState . runErrorT

-- | Computed config data

data ShardStat = ShardStat {resident :: Int, movingOff :: MCount, movingOn :: MCount}
	deriving (Show, Eq)
data MCount = MCount {moving :: Int, waiting :: Int} deriving (Show, Eq)
zeroStat = ShardStat 0 (MCount 0 0) (MCount 0 0)
incr_resident s = s {resident = resident s + 1}
incr_movingOff b s = s {movingOff = incr_count b (movingOff s)}
incr_movingOn b s = s {movingOn = incr_count b (movingOn s)}
incr_count b c = if b then c {moving = moving c + 1} else c {waiting = waiting c + 1}

shardStats :: Database -> Map ShardNum ShardStat
shardStats = fold gather M.empty . chunks where
	gather (ChunkData s _ mv) = case mv of
		Nothing -> up incr_resident s
		Just (Moving s' m) -> up (incr_movingOff (m > 0)) s . up (incr_movingOn (m > 0)) s'
	up f = alter (Just . f . maybe zeroStat id)

data ShardData = ShardData {draining :: Bool, sStat :: ShardStat} deriving (Show)
mod_sStat f d = d {sStat = f (sStat d)}

shardData :: Database -> Map ShardNum ShardData
shardData = zipWith ShardData False zeroStat <$> shards <*> shardStats

-- | Sharding operations

addShard :: M ()
addShard = do
	s <- get_shards $ maybe 0 fst . findMaxM
	modify_shards $ insert (s + 1) False

removeShard :: ShardNum -> M ()
-- ^ Mark shard as draining & abort moving chunks destined for this shard but not yet started.
-- 'balanceChunks' will move chunks off and delete shard when completely drained.
removeShard s = do
	guard =<< get_shards ((1 <) . size . filter not)
	modify_shards $ insert s True
	modify_chunks $ mapSome ((Just (Moving s 0) ==) . cMoving) (set_cMoving Nothing)

splitChunk :: ChunkKey -> M ()
splitChunk ck = do
	(k, d) : cs <- get_chunks $ take 2 . list . gte ck
	guard (k == ck)
	when (isNothing (cMoving d) && cCount (cStat d) > 1) $ do
		let (s1, k2, s2) = splitIt k (cStat d) (fst <$> listToMaybe cs)
		modify_chunks $
			adjust (set_cStat s1) k .
			insert k2 (ChunkData (cShard d) s2 Nothing)
  where
  	mid a b = (a + b) `div` 2
	splitIt k1 (ChunkStat siz cnt sum) mk3 = (stat mid1, k2, stat mid2) where
		k2 = sum `div` cnt  -- avg key is split point
		mid1 = mid k1 k2  -- new middle keys for lower & upper half
		mid2 = maybe (k2 + k2 - mid1) (mid k2) mk3
		stat midK = ChunkStat (siz / 2) (cnt `div` 2) (cnt * midK `div` 2)

moveChunk :: ChunkKey -> ShardNum -> M ()
-- ^ Start moving chunk to given shard (if not already moving).
-- 'migrateChunks' will continue moving the chunk
moveChunk c s = do
	Just d <- get_chunks $ lookup c
	guard (cShard d /= s && cMoving d == Nothing)
	modify_chunks $ adjust (set_cMoving $ Just (Moving s 0)) c

maxImbalance = 8  -- ^ difference in shards' chunk count before moving chunks to balance
maxWaiting = 2  -- ^ max chunks waiting to start move to/from each shard

balanceChunks :: M ()
-- ^ For each shard in "loaded" order, move chunks that are "over balance" until maxWaiting is 
-- reached on either side. "Loaded" order is draining shards then remaining shards in decreasing 
-- chunk count order. "Over balance" are draining shards or shards with maxImbalance more chunks 
-- then the intended destination.
balanceChunks = do
	sData <- gets shardData
	let ss = fst <$> L.sortBy (flip $ comparing load) (list sData)
	foldM_ (balance ss) sData ss
	modify_shards (`diff` drainedShards sData)
  where
	load (_, sd) = if draining sd then maxBound else srcCount (sStat sd)
	srcCount (ShardStat r _off (MCount nm nw)) = r + nm + nw
	dstCount (ShardStat r (MCount rm rw) (MCount nm nw)) = r + rm + rw + nm + nw
	overBalance sd td = srcCount (sStat sd) - dstCount (sStat td) > maxImbalance
	balance ss sData s = mfold (moveBunch s) sData (reverse ss)
	moveBunch s sData t = do
		guard $ s /= t 					-- stop mfold loop
		guard $ not (draining (sData ! t))
		miterate (moveOne s t) sData
	moveOne s t sData = do
		guard $ draining (sData ! s) || overBalance (sData ! s) (sData ! t)
		guard $ (waiting . movingOn . sStat) (sData ! t) <= maxWaiting
		guard $ (waiting . movingOff . sStat) (sData ! s) <= maxWaiting
		Just (c, _) <- pickChunkIn s
		moveChunk c t
		return $ flip ($) sData $
			adjust (mod_sStat $ incr_movingOff False) s .
			adjust (mod_sStat $ incr_movingOn False) t
	pickChunkIn s = get_chunks $ findMinM . filter (\d -> cShard d == s && cMoving d == Nothing)
	drainedShards = keysSet . filter (\d -> draining d && sStat d == zeroStat)

maxChunkSize = 64 :: MB

data Document = Doc {dKey :: Key, dSize :: MB }

insertDocument :: Document -> M ()
insertDocument doc = do
	guard (dKey doc >= 0 && dSize doc > 0)
	mc <- get_chunks $ findMaxM . lte (dKey doc)
	(c, ChunkData _ (ChunkStat siz cnt sum) _) <- maybe initZeroChunk return mc
	modify_chunks $ flip adjust c $
		set_cStat $ ChunkStat (siz + dSize doc) (cnt + 1) (sum + dKey doc)
	when (siz + dSize doc > maxChunkSize) (splitChunk c)
  where
	initZeroChunk = do
		Just (s, _) <- get_shards findMinM
		let d = ChunkData s (ChunkStat 0 0 0) Nothing
		modify_chunks $ insert 0 d
		return (0, d)

networkBandwidth = 80 :: MBps  -- ^ per shard each way

migrateChunks :: Sec -> M ()
-- ^ Advance moving chunks by given secs according to networkBandwidth for each shard
migrateChunks t = do
	movers <- get_chunks $ list . filter (isJust . cMoving)
	foldM_ advance (M.empty, M.empty) movers
  where
	netCap = networkBandwidth * t :: MB
	advance (inCap, outCap) (c, d) = do
		modify_chunks $ flip adjust c $ if mvSoFar v + sent >= cSize (cStat d)
			then set_cShard (mvTo v) . set_cMoving Nothing
			else set_cMoving $ Just v {mvSoFar = mvSoFar v + sent}
		return (inCap', outCap')
	  where
	  	Just v = cMoving d
		outC = M.findWithDefault netCap (cShard d) outCap
		inC = M.findWithDefault netCap (mvTo v) inCap
		sent = minimum [outC, inC, cSize (cStat d) - mvSoFar v]
		outCap' = M.insert (cShard d) (outC - sent) outCap
		inCap' = M.insert (mvTo v) (inC - sent) inCap
