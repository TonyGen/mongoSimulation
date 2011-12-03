-- | Run MongoSharding simulation showing animated graphics of chunks moving between shards.
-- See README.md for more detailed description.

{-# LANGUAGE TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Main (main) where

import MongoSharding
import Graphics.PictureUtil
import Graphics.Gloss.Interface.Game
import Control.Applicative ((<$>), (<*>))
import qualified Data.Map as M
import qualified Data.MapUtil as M
import qualified Data.List as L
import Data.Maybe (fromJust)
import System.Random
import Control.Monad

helpText = [
	"a -> add shard",
	"r -> remove shard under mouse",
	"s -> start/stop insert documents",
	"space -> show/hide help" ]

showHelp = pictures $ zipWith f [0..] helpText where
	f i = translate (-220) (70 - i * 40) . scale 0.2 0.2 . text

main = do
	rs :: [Integer] <- randomRsIO (0, 2^64)
	gameInWindow "MongoDB Sharding"
		(600, 400) (400, 200) white
		(1000 `div` stepIntervalMs) (state rs) picture event step

randomRsIO :: (Random a) => (a, a) -> IO [a]
randomRsIO range = randomRs range <$> newStdGen

state rs = (emptyDb, rs, False, True)
-- ^ (Database, RandomSupply, continuously insert docs?, show help?)

picture (db, _, _, help) = if help
	then showHelp
	else pictures (shardPictures db)

shardPictures db = zipWith shardPic [0..] shards where
	shardPic i s = translate (leftMargin + i * 50 + 25) 0 (shardPicture 40 chunks s)
	Right chunks = evalM (get_chunks M.list) db
	Right shards = evalM (get_shards M.list) db
	leftMargin = realToFrac (length shards) * (-50) / 2

shardPicture width chunks (sn, draining) = pictures (box : chunkLines) where
	myChunks = filter (\(_, d) -> cShard d == sn || (mvTo <$> cMoving d) == Just sn) chunks
	height = realToFrac $ length myChunks * 2 + 10
	box = color (if draining then orange else violet) (rectangleWire width height)
	chunkLines = L.map chunkLine $ zip [0..] myChunks
	chunkLine (i, (c, d)) = translate 0 (i*2 - height/2 + 5) (color col bar) where
		bar = rectangleSolid (width * sz / maxChunkSize) 1
		(sz, col) = if cShard d == sn
			then maybe (cSize (cStat d), black) ((,red) . (cSize (cStat d) -) . mvSoFar) (cMoving d)
			else (mvSoFar (fromJust (cMoving d)), dark green)

event (EventKey key Down _ mouse) (db, rs, idoc, help) = case key of
	Char 'a' -> (execM addShard db, rs, idoc, False)
	Char 'r' -> (maybe id remove mi $ db, rs, idoc, False) where
		remove = execM . removeShard . fst . (shards !!)
		Right shards = evalM (get_shards M.list) db
		mi = L.findIndex (pictureContains mouse) (shardPictures db)
	Char 's' -> (db, rs, not idoc, False)  -- toggle inserting docs
	SpecialKey KeySpace -> (db, rs, idoc, not help)
	MouseButton _ -> (db, rs, idoc, not help)
	_ -> (db, rs, idoc, True)
event _ x = x

stepIntervalMs = 10  -- milliseconds
-- ^ Run step function below every 10 milliseconds

step secs (db, (r1 : r2 : rs), idoc, help) = (execM run db, rs, idoc, help) where
	run = when idoc (insertDoc r1 r2) >> balanceChunks >> migrateChunks secs

insertDoc r1 r2 = insertDocument (Doc (key r1) (size r2)) where
	key = id
	size r = logBase 2 (realToFrac r) / 100  -- up to 640KB
