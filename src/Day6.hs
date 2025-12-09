{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day6 (day6, parseOp) where

import Data.List.Split (splitWhen)
import Data.List (transpose)
import Data.String.Utils (strip)

day6 :: IO ()
day6 = do 
    input <- readFile "/home/philip/git/aoc2025/inputs/day6.txt" 
    let equations = parseInput input 
    let part1 = solve1 equations
    print part1 
    let equations2 = parseInput2 input
    let part2 = solve2 equations2
    print part2 

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


parseOp :: Num a => String -> a -> a -> a
parseOp "+" = (+)
parseOp "*" = (*)

parseInput2 :: String -> [[String]]
parseInput2 = splitWhen (== "") . map strip . transpose . lines 
-- = [["1  *","24","356"],["369+","248","8"],["32*","581","175"],["623+","431","4"]]

solve2 :: [[String]] -> Int 
solve2 = foldr calcEq2 0 

calcEq2 :: [String] -> Int -> Int 
calcEq2 eq acc = acc + foldr op start eqNums
    where op = parseOp $ (: []) $ last $ head eq 
          eqNums = map (read . takeWhile (\c -> c /= '*' && c /= '+')) eq 
          start
            | (last $ head eq) == '+' = 0
            | otherwise = 1  
