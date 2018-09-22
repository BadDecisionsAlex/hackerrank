import Control.Monad

tree :: Integer -> Integer
tree n = (sum ([ treeL 1 t n | t <- [ 1..n ] ])) `mod` (10^8 + 7)

treeL :: Integer -> Integer -> Integer -> Integer
treeL l m r
    | l == m && m == r = 1
    | l == m           = right
    | m == r           = left
    | otherwise        = left * right
    where
        right = sum ([ treeL (m + 1) mr r | mr <- [ (m + 1)..r ] ])
        left  = sum ([ treeL l ml (m - 1) | ml <- [ l..(m - 1) ] ])

main = do
    t <- fmap (read :: String -> Int) getLine
    forM_ [1..t] (\_ -> do
        i <- fmap (read :: String -> Integer) getLine
        putStrLn $ show $ tree i
        )
