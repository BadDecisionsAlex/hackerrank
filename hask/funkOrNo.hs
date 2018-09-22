-- Given a list of (x, y) pairs, determine if a function is valid.
-- Ensure that each x is unique.
--  ex) (2, 0) and (2, 1) cannot both be points

import Data.Char
import Data.List
import Control.Monad

type Point = (Int, Int)

solveSet = do
    n <- getLine
    points <- makePts (read n)
    if all (isValidPt points) points
        then putStrLn "YES"
        else putStrLn "NO"

isValidPt :: [ Point ] -> Point -> Bool
isValidPt set (x, y) = all ((== y) . snd) $ filter ((== x) .fst) set

makePt :: String -> Point
makePt line = (\[x, y] -> (read x, read y)) $ words line

makePts :: Int -> IO [ Point ]
makePts n = do lines <- sequence $ replicate n getLine
               let points = map makePt lines
               return points

main = do
    t <- getLine
    sequence $ replicate (read t) solveSet

-- Suggested template:

valid :: [ Point ] -> Bool
valid f = True -- code goes here --

m = do
    t <- fmap (read :: String -> Int) getLine
    forM [1..t] (\_ -> do
        n <- fmap (read :: String -> Int) getLine
        func <- forM [1..n] (\_ -> do
            fmap ((\[a, b] -> (a, b)) . map (read :: String -> Int) . words) getLine :: IO Point)
        putStrLn $ if valid func then "YES" else "NO")
