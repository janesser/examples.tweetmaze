-----------------------------------------------------------------------------
--
-- Module      :  Tweetmaze
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-- http://www.lispology.com/show?ZXE
--
--
-----------------------------------------------------------------------------

module Tweetmaze (

) where

type Puzzle = [Int]
type Result = [Int]

solvePuzzle :: String -> Result
solvePuzzle str = solve (readPuzzle str) where
    readPuzzle [] = []
    readPuzzle (x:xs) = [read [x]] ++ (readPuzzle xs)

solve :: Puzzle -> Result
solve puzzle = solveP [[0]] where
    solveP ss
        | length completed > 0 = head completed
        | otherwise = solveP (nextGen ss) where
        completed = filter (\s -> last s == (length puzzle) - 1) ss
        nextGen ss = (map left ss) ++ (map right ss) where
            left s = jump s (-) (\x -> x >= 0)
                -- | (last s) - (puzzle !! (last s)) >= 0 = s ++ [(last s) - (puzzle !! (last s))]
                -- | otherwise = s
            right s = jump s (+) (\x -> x < length puzzle)
                -- | (last s) + (puzzle !! (last s)) < length puzzle = s ++ [(last s) + (puzzle !! (last s))]
                -- | otherwise = s
            jump s op chk
                | (chk (op sl x)) = s ++ [op sl x]
                | otherwise = s where
                sl = last s
                x = puzzle !! sl
