import           Control.Monad.State
import           Data.List
import           Data.List.Split
import           System.Environment


passphraseValid :: (String -> String) -> [String] -> State (Bool, [String]) Bool
passphraseValid _ []       = get >>= return . fst
passphraseValid f (x : xs) = do
    (b, e) <- get
    let x' = f x
    case b of
        True  -> put (not (x' `elem` e), e ++ [x'])
        False -> put (False, e ++ [x'])
    passphraseValid f xs

main :: IO ()
main = do
    content <- readFile "day4.txt"
    let lines = splitOn "\n" content
    -- Tackling for problem 1 or problem 2
    let p1 = passphraseValid id
    let p2 = passphraseValid sort
    let validNum = foldl (\x y ->
                            if length y > 1 && evalState (p2 y) (True, [])
                                then x + 1
                                else x
                         ) 0 (map (splitOn " ") lines)
    print $ validNum
