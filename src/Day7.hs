{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day7 (day7, parseLine, calcBeams) where

import qualified Data.Set as S

day7 :: IO ()
day7 = do
    input <- readFile "/home/philip/git/aoc2025/inputs/day7.txt"
    let manifold = lines input
    let start = parseLine 'S' $ head manifold
    print start
    let splitters = map (parseLine '^' . snd) (filter (even . fst) $ zip [1..] $ tail manifold)
    -- print splitters
    let result = calcBeams start 0 splitters 
    print result

parseLine :: Char -> String -> S.Set Int
parseLine c = S.fromList . map fst . filter (\x -> c == snd x) . zip [0..]

calcBeams :: S.Set Int -> Int -> [S.Set Int] -> Int 
calcBeams _ count [] = count
calcBeams beams count (x:xs) = calcBeams updateBeams updateCount xs 
    where splittersHit = S.intersection beams x
          updateCount = S.size splittersHit + count
          beamsNoHit = S.difference beams splittersHit
          beamsSplit = S.fromList $ concatMap (\x -> [x+1,x-1]) $ S.toList splittersHit
          updateBeams = S.union beamsNoHit beamsSplit