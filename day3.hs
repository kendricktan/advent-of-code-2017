import           Data.Char
import           System.Environment


manhattanDist1 :: Int -> Int
manhattanDist1 1 = 0
manhattanDist1 x = (l `div` 2) + (abs ((l `div` 2) - d))
    where l' = (ceiling $ sqrt (fromIntegral x)) :: Int -- Length of the square/rectangle
          l = if odd l' then l' else l' + 1             -- Length of the square
          c = foldl (\x' y' -> if (x' - y') >= x then (x' - y') else x') (l*l) [l-1, l-1, l-1, l-1] -- Which side is it closest to
          d = c - x

main :: IO ()
main = do
    (x : _) <- getArgs
    print $ manhattanDist1 (read x :: Int)
