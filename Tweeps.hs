-----------------------------------------------------------------------------
--
-- Module      :  Tweeps
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-- puzzle given like [3,4,5,1]
--
-- find shortest way from first to last element
--
-- 1. may jump number of elements like current element's value
-- 2. may step to right neighbour
-----------------------------------------------------------------------------

module Tweeps (

) where

data Tweep = [Int]
data Solution = [Int]

skip i nums
    | i > 0 = skip (i - 1) (tail nums)
    | otherwise = nums

step nums = skip 1 nums

canJump (x:xs) = x <= length(xs)
jump (x:xs)
    | canJump (x:xs) = skip (x - 1) xs

-- always step
--- fail-safe
simpleSolve nums = solve 0 nums where
    solve i [] = []
    solve i nums = [i] ++ (solve (i + 1) (step nums))

-- jump as much as possible
--- better than simple if exists i: nums !! i > 1
naivSolve nums = [0] ++ (solve 0 nums) where
    solve i [x] = []
    solve i (x:xs)
        | canJump (x:xs) = [i + x] ++ (solve (i + x) (jump (x:xs)))
        | otherwise = [i + 1] ++ (solve (i + 1) xs)

-- maximize reverse jumps
--- better than naiv on e.g. [2,3,1,1,1]
--- worse than naiv on e.g. [3,1,3,2,1,1]
reverseSolve nums = (solve 0 nums) ++ [(length nums) - 1] where
    solve i [x] = []
    solve i nums
        | i >= length(nums) - 2 = (solve 0 (rskip i nums)) ++ [i]
        | i + (nums !! i) == length(nums) - 1 = (solve 0 (rskip i nums)) ++ [i]
        | otherwise = solve (i + 1) nums
    rskip i [] = []
    rskip i nums = reverse (skip ((length nums) - i - 1) (reverse nums))

shortest (x:xs) = find x xs where
    find way [] = way
    find way (x:xs)
        | length(x) < length(way) = find x xs
        | otherwise = find way xs

combinedSolve nums = shortest (map (\f -> (f nums)) [naivSolve, reverseSolve])

-- greedy approach
--- first completed routes are shortest
parallelSolve nums = solve [([0], nums)] where
    solve routeNums
        -- 3. result
        | (length completedRoutes) > 0 = map fst completedRoutes
        | otherwise = solve nextGenRoutes where
        -- 2. filter
        completedRoutes = filter (\x -> (length (snd x)) <= 1) nextGenRoutes
        -- 1. diverge
        nextGenRoutes = (map stepIter routeNums) ++ (map jumpIter routeNums)
        stepIter (route, nums) = (route ++ [(last route) + 1], step nums)
        jumpIter (route, nums)
            | canJump nums = (route ++ [(last route) + (head nums)], jump nums)
            | otherwise = (route, nums) -- unchanged, alt: stepIter (route, nums)
