module Day2 (day2) where

import Data.List.Split (splitOn)
import Text.Regex.PCRE ((=~))

day2 :: IO ()
day2 = do 
    input <- readFile "/home/philip/git/aoc2025/inputs/day2.txt"
    let part1 = solve1 input
    print part1

solve1 :: String -> Int  
solve1 =  sum . map read . filter (\s -> s =~ pattern ) . concatMap (\s -> rangeToList s) . splitOn ","
    where rangeToList  = map show . (\([start,end]) -> [start..end]) . map (\s -> read s :: Int) . splitOn "-"
          pattern = "^(\\d+)\\1$"
