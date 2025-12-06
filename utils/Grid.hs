module Grid (
    Grid(..), 
    fromList, ofSize, size, get, inBounds, coordinates, values,
    cardinalDirections, allDirections, getNeighborCoordinates, getNeighborValues
) where

import Data.Maybe

type Grid a = [[a]]

type Coord = (Int, Int)

fromList :: [[a]] -> Grid a 
fromList = id 

ofSize :: Int ->  Grid String
ofSize n = replicate n defaultRow 
    where defaultRow = replicate n ""

size :: Grid a -> Coord
size [] = (0,0)
size g@(row:_) = (length g, length row)

get :: Grid a -> Coord -> Maybe a 
get grid (r, c)
    | inBounds grid (r,c) = Just (grid !! r !! c )
    | otherwise = Nothing

inBounds :: Grid a -> Coord -> Bool
inBounds grid (r, c) = r >= 0 && r < rows && c >= 0 && c < cols 
    where (rows, cols) = size grid 

coordinates :: Grid a -> [Coord]
coordinates grid = [(r,c) | r <- [0..rows], c <- [0..cols]]
    where (rows, cols) = size grid

values :: Grid a -> [a]
values grid = [grid !! r !! c | r <- [0..rows], c <- [0..cols]]
    where (rows, cols) = size grid

cardinalDirections :: [Coord]
cardinalDirections = [(-1,0), (1,0), (0,-1), (0,1)]

allDirections :: [Coord]
allDirections = cardinalDirections ++ [(-1,-1), (1,-1), (-1,1), (1,1)]

getNeighborCoordinates :: Grid a -> [Coord] -> Coord -> [Coord]
getNeighborCoordinates grid directions (r,c) = filter (inBounds grid) $ map (\(dr,dc) -> (r + dr, c + dc)) directions 

getNeighborValues :: Grid a -> [Coord] -> Coord -> [a]
getNeighborValues grid directions (r,c) = mapMaybe (get grid) neighbors 
    where neighbors = getNeighborCoordinates grid directions (r,c)