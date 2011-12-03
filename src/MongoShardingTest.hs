-- | Run MongoSharding simulation without graphics

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import MongoSharding
import System.Random
import Control.Monad
import Data.Map
import Control.Concurrent (threadDelay)
import Control.Applicative ((<$>))

main = do
	rs :: [Integer] <- randomRsIO (0, 2^64)
	let db = execM (addShard >> addShard >> addShard) emptyDb
	foldM_ run (db, rs) [0 .. 60 * 100]

run (db, rs) i = do
	threadDelay (10 * 1000)
	when (i `mod` (5 * 100) == 0) $ do
		mapM_ print $ toList (shardData db)
		putStrLn ""
	return $ step 0.01 (db, rs)

randomRsIO :: (Random a) => (a, a) -> IO [a]
randomRsIO range = randomRs range <$> newStdGen

step secs (db, (r1 : r2 : rs)) = (execM run db, rs) where
	run = insertDoc r1 r2 >> balanceChunks >> migrateChunks secs

insertDoc r1 r2 = insertDocument (Doc (key r1) (size r2)) where
	key = id
	size r = logBase 2 (realToFrac r) / 100  -- up to 640KB
