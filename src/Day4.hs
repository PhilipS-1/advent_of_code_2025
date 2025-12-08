{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day4 (day4) where

import Grid 

day4 :: IO ()
day4 = do 
    input <- readFile "/home/philip/git/aoc2025/inputs/day4.txt"
    let grid = Grid.fromList (lines input) 
    let part1 = solve1 grid
    print part1
    let part2 = solve2 grid 0
    print part2 

solve1 :: Grid Char -> Int 
solve1 grid = length $ filter isAccessable $ map (Grid.getNeighborValues grid Grid.allDirections) $ filter hasPaperRoll allCoords 
    where allCoords = Grid.coordinates grid
          hasPaperRoll cell = Grid.get grid cell == Just '@'
          isAccessable neighbors =  (4 >) $ length $ filter (== '@') neighbors

solve2 :: Grid Char -> Int  -> Int 
solve2 grid count
    | 0 == countAccessable grid = count 
    | otherwise = solve2 updatedGrid (count + countAccessable grid)
    where countAccessable = solve1
          updatedGrid = Grid.updateGrid grid (cellsToRemove grid) '.'

cellsToRemove :: Grid Char -> [Coord]
cellsToRemove grid = filter isAccessible $ filter hasPaperRoll allCoords
    where allCoords = Grid.coordinates grid
          hasPaperRoll coord = Grid.get grid coord == Just '@'
          isAccessible coord = (4 >) $ length $ filter (== '@') $ Grid.getNeighborValues grid Grid.allDirections coord