import           Data.Char
import           Data.Map
import           Prelude
import           System.Environment


manhattanDist1 :: Int -> Int
manhattanDist1 1 = 0
manhattanDist1 x = (l `div` 2) + (abs ((l `div` 2) - d))
    where l' = (ceiling $ sqrt (fromIntegral x)) :: Int -- Length of the square/rectangle
          l = if odd l' then l' else l' + 1             -- Length of the square
          c = Prelude.foldl (\x' y' -> if (x' - y') >= x then (x' - y') else x') (l*l) [l-1, l-1, l-1, l-1] -- Which side is it closest to
          d = c - x

-- Index -> Number
-- There's a number of cases:
--      case 1: you add your (idx - 1) and (idx - 2) the 2 numbers from the square below
--      case 2: where you only add value at (idx - 1) and ((l - 1)^2) - (l-2)-(l-1)-(l-1)-(l-1))
--      case 3: you're at the corner, add number before you and the same corner from the square below
--      case 4: you add 1 of your previous numbers, and the 2 numbers from the square below
--      case 5: you add 1 of your previous numbers, and the 2 numbers from the square below
--      case 5: you add 2 of your previous numbers, and the 3 numbers from the square below
manhattanDist2 :: Int -> Int
manhattanDist2 1 = 1
manhattanDist2 2 = 1
manhattanDist2 3 = 2
manhattanDist2 4 = 4
manhattanDist2 5 = 5
manhattanDist2 6 = 10
manhattanDist2 7 = 11
manhattanDist2 8 = 23
manhattanDist2 9 = 25
manhattanDist2 idx = n
    where l' = (ceiling $ sqrt (fromIntegral idx)) :: Int -- Length of the square/rectangle
          l = if odd l' then l' else l' + 1               -- Length of the square
          lprev = l - 2                                   -- Previous length of square
          startIdxL = (l^2)-(4*(l - 1)) + 1               -- Starting of the index of the current square
          idxPrev = Prelude.foldl (\x _ -> if (x > startIdxL) then x - (l - 1) else x - (lprev - 1)) idx [1..4] -- Index of the previous square relative to current index
          isCase1 = Prelude.foldl (\x y -> x || (idx == y)) False [l*l-(l-2)-(x*(l-1)) | x <- [0..2]]
          isCase2 = idx == (l*l-(l-2)-(3*(l-1)))
          isCase3 = Prelude.foldl (\x y -> x || (idx == y)) False [l*l-(x*(l-1)) | x <- [1..3]]
          isCase4 = Prelude.foldl (\x y -> x || (idx == y)) False ([l*l-(x*(l-1))-1 | x <- [1..3]] ++ [l*l])
          isCase5 = idx == l*l-1
          isCase6 = idx == lprev^2+2
          n = if isCase1 then manhattanDist2 (idx - 1) + manhattanDist2 (idx - 2) + manhattanDist2 idxPrev + manhattanDist2 (idxPrev - 1)
                else if isCase2 then manhattanDist2 (idx - 1) + manhattanDist2 ((lprev^2) - (lprev-2)-(3*(lprev-1)))
                else if isCase3 then manhattanDist2 (idx - 1) + manhattanDist2 (idxPrev)
                else if isCase4 then manhattanDist2 (idx - 1) + manhattanDist2 idxPrev + manhattanDist2 (idxPrev + 1)
                else if isCase5 then manhattanDist2 (idx - 1) + manhattanDist2 idxPrev + manhattanDist2 (idxPrev + 1) + manhattanDist2 (idxPrev + 2)
                else if isCase6 then manhattanDist2 (idx - 2) + manhattanDist2 (idx -1) + manhattanDist2 (idxPrev + 1) + manhattanDist2 (idxPrev + 2)
                else manhattanDist2 (idx - 1) + manhattanDist2 idxPrev + manhattanDist2 (idxPrev + 1) + manhattanDist2 (idxPrev + 2)


main :: IO ()
main = do
    (i : _) <- getArgs
    let i' = last $ takeWhile (\x -> manhattanDist2 x < (read i :: Int)) [1..]
    print $ manhattanDist2 (i' + 1)
    -- print $ manhattanDist1 (read x :: Int)
