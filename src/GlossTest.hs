-- | Empty gloss game consumes increasing CPU until it reached 100% after one hour.

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Graphics.Gloss.Interface.Game

main = gameInWindow "CPU always increasing"
		(600, 400) (400, 200) white
		100 initial picture event step

picture rs = text "x"

event _ = id

initial = [0..] :: [Integer]

step _ (n1 : n2 : ns) = ns
