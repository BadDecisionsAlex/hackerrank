-- Given a Polygon determine it's perimeter.

import Data.List
import Control.Monad

type Point = (Float, Float)

dist :: Point -> Point -> Float
dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

perim :: [ Point ] -> Float
perim (_:[]) = 0
perim (a:b:xs) = (dist a b) + perim (b:xs)

area :: [ Point ] -> Float
area [] = 0.0
area points = let shifted = [ points ] >>= (\x -> tail x ++ [head x])
                  areas = zipWith (\(x1, y1) (x2, y2) -> (x1 * y2 - x2 * y1) / 2.0) points shifted
              in sum $ areas
--area points = 0.5 * (perim $ points ++ [head points]) * (apoth points)

midpoint :: [ Point ] -> Point
midpoint points =
    (\l (x, y) -> (x / l, y / l )) (realToFrac (length points))
        (foldl (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0.0, 0.0) points)

-- Goes from Polygon's center, to the midpoint of any of it's sides.
apoth :: [ Point ] -> Float
apoth points = dist (midpoint points) $ midpoint (take 2 points)

main = do
    n <- fmap (read :: String -> Int) getLine
    points <- forM [1..n] (\_ -> do
        fmap ((\[a, b] -> (a, b)) . map (read :: String -> Float) . words) getLine :: IO Point)
    putStrLn $ show $ area points
    -- putStrLn $ show $ perim $ points ++ [ head points ]
