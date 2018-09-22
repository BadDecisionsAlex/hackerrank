
import Control.Monad

fib :: Integer -> Integer
fib n = fibz n [ 0, 0, 1 ]
    where
    fibz :: Integer -> [ Integer ] -> Integer
    fibz 0 (_:y:_)     = y
    fibz n (x:y:z:(_)) = fibz (n - 1) [ y, z, y + z ]

main = do
    t <- fmap (read :: String -> Int) getLine
    forM_ [1..t] (\_ -> do
        i <- fmap (read :: String -> Integer) getLine
        putStrLn $ show $ (fib i) `mod` (10^8 + 7)
        )
