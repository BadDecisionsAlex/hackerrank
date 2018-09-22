import Data.List
import Data.Char
import System.IO
import Control.Monad

main = do
    c <- getContents
    let chunks i l = (\(h,t) -> h:(if t/= [] then chunks i t else [])) (splitAt i l)
    sequence_ $ map (\(b:w:[]) -> do
        putStrLn $ "Breaks: " ++ b ++ "\nWord Lengths: [ " ++ w ++ " ]"
        putStrLn $ "Shortest Line: " ++ (show $ solve (map read $ words w) (read b))
        ) $ chunks 2 $ lines c
 
solve wordLengths breaks = minimum $ map sum $ concat evalWinners where
    rough = (sum wordLengths) `div` (breaks+1)
    sections = (!!0) $ forM [1..breaks] sect where
        sect b = return [i, i+1] where
            i = length.fst $ break (> (rough * b)) $ scanl1 (+) wordLengths
    combinations = combs [[]] sections where
        combs done (choice:cs) = let d = comb done choice in if cs == [] then d else combs d cs
        comb d c = [ d' ++ [c'] | c' <- c, d' <- d ]
    partAt indices = ptat [] indices wordLengths where
        ptat d (i:[]) l = let p = splitAt i l in d ++ [fst p, snd p]
        ptat d (i:is) l = let p = splitAt i l in ptat (d ++ [(fst p)]) (map (\x -> (-) x i) is) (snd p)
    partitions = map partAt combinations
    eval pts = let s = map sum pts in sum [ abs (x-y) | x <- s, y <- s ]
    evalWinners = map snd $ filter (\x -> (fst x) == (fst $ minimum pairs)) pairs where
        pairs = map (\p -> (eval p, p)) partitions
