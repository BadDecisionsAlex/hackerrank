import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Double -> Double -> [ Double ] -> [ Double ] -> [ Double ]
solve l r a b = [ (area l r a b), (volume l r a b) ]

integral f l r = sum [ (f x) * dx | x <- [ l, l + dx .. r ] ]
    where dx = 0.001

poly :: [ Double ] -> [ Double ] -> Double -> Double
poly a b x = sum [ ac * (x ** bc) | (ac, bc) <- zip a b ]

rotate f x = y * y * pi
    where y = f x

area l r a b = integral (poly a b) l r

volume l r a b = integral (rotate (poly a b)) l r

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map (read :: String -> Double). words). lines
