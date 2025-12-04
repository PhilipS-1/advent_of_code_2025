{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day3 (day3) where

day3 :: IO ()
day3 = do 
    input <- readFile "/home/philip/git/aoc2025/inputs/day3.txt"
    let part1 = solve1 input
    print part1
    let part2 = solve2 input 
    print part2

solve1 :: String -> Int
solve1 =  sum . map (read . findLargest 2) . lines 

solve2 :: String -> Int 
solve2 = sum . map (read . findLargest 12) . lines 

findLargest :: Int -> String ->  String  
findLargest 0 _ = ""
-- findLargest _ "" = ""
findLargest n line = maxDigit : findLargest (n-1) restOfList
    where maxDigit = maximum $ take (length line - (n - 1)) line 
          restOfList = tail (dropWhile (/= maxDigit) line) 