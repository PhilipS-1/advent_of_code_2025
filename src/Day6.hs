{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day6 (day6, parseOp) where

import Data.List (transpose)

day6 :: IO ()
day6 = do 
    input <- readFile "/home/philip/git/aoc2025/inputs/day6.txt" 
    let equations = parseInput input 
    let part1 = solve1 equations
    print part1 

parseInput :: String -> [[String]]
parseInput = transpose . map words . lines

solve1 :: [[String]] -> Int 
solve1 = foldr calcEq 0

calcEq :: [String] -> Int -> Int 
calcEq eq acc = acc + foldr op start eqNums
    where op = parseOp (last eq)
          eqNums = map read (takeWhile (\s -> s /= "+" && s /= "*") eq) 
          start
            | last eq == "+" = 0
            | otherwise = 1 

parseOp :: Num a => String -> (a -> a -> a)
parseOp "+" = (+)
parseOp "*" = (*)