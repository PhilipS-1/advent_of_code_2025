{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day5 (day5, mergeIntervals) where

import Data.List.Split (splitOn)
import Data.List (sort)

day5 :: IO ()
day5 = do 
    input <- readFile "/home/philip/git/aoc2025/inputs/day4.txt"
    let [ranges, ids] = map (splitOn "\n") $ splitOn "\n\n" input
    let part1 = solve1 ranges ids
    print part1 
    let part2 = solve2 ranges 
    print part2

solve1 :: [String] -> [String] -> Int 
solve1 ranges ids = length $ filter (inRange rangeList) ids  
    where rangeList = map ((\[start, end] -> (start, end))  . splitOn "-") ranges
          inRange rangeList id = any (\x -> intId >= fst x && intId <= snd x) intRanges
            where intId = read id :: Int 
                  intRanges = map(\(a,b) -> (read a :: Int, read b :: Int)) rangeList

solve2 :: [String] -> Int 
solve2 ranges =  foldr (\(start, end) acc -> end - start + 1 + acc) 0 mergedRanges 
    where mergedRanges :: [(Int, Int)]
          mergedRanges = mergeIntervals $ parseToIntTuples ranges
            where parseToIntTuples = map (\[start, end] -> (start, end)) . map (map (\x -> read x :: Int) . splitOn "-")

mergeIntervals :: [(Int, Int)] -> [(Int, Int)]
mergeIntervals [] = []
mergeIntervals xs = foldl merge [] sortedXs 
    where sortedXs = sort xs 

merge :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
merge [] range = [range]
merge (current@(start1, end1):rest) next@(start2, end2)
    | start2 <= end1 = (start1, max end1 end2) : rest 
    | otherwise = next : current : rest 