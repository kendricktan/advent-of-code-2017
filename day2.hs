import           Control.Monad
import           Data.Char
import           Data.List.Split
import           System.Environment

checksum :: [String] -> Integer -> Integer -> Integer
checksum [] nmin nmax       = max (nmax - nmin) 0
checksum (x : xs) nmin nmax = checksum xs nMin nMax
    where i = read x :: Integer
          nMin = if (i < nmin) then i else nmin
          nMax = if (i > nmax) then i else nmax


checksum2 :: [String] -> Integer
checksum2 s = sum $ liftM2 (\x y -> if x == y
                                 then 0
                                 else
                                     case (if (x > y) then (x `mod` y) else (y `mod` x)) of
                                           0 -> if (x > y) then (x `div` y) else (y `div` x)
                                           _ -> 0
                           ) integers integers
    where integers = map (\x -> read x :: Integer) s


main :: IO ()
main = do
    content <- readFile "day2.txt"
    let linesOfFiles = map (\x -> splitOn "\t" x) (lines content)
    print $ div (sum $ map checksum2 linesOfFiles) 2 -- div 2 cause we find the same number twice
    -- print $ foldl (\x y -> x + (checksum y 9999 0)) 0 linesOfFiles
