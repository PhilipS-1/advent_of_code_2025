{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day5 (day5) where

import Data.List.Split (splitOn)

day5 :: IO ()
day5 = do 
    input <- readFile "/home/philip/git/aoc2025/inputs/day4.txt"
    let (ranges, ids) = parseInput $ splitOn "\n\n" input
    let part1 = solve1 ranges ids
    print part1 


parseInput :: [String] -> ([String], [String])
parseInput = toTuple . map (splitOn "\n")
    where toTuple [x,y] = (x, y)

solve1 :: [String] -> [String] -> Int 
solve1 ranges ids = length $ filter (inRange rangeList) ids  
    where rangeList = map ((\[start, end] -> (start, end))  . splitOn "-") ranges
          inRange rangeList id = any (\x -> intId >= fst x && intId <= snd x) intRanges
            where intId = read id :: Int 
                  intRanges = map(\(a,b) -> (read a :: Int, read b :: Int)) rangeList