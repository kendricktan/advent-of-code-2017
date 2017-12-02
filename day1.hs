import           Data.Char
import           System.Environment

sumCaptcha :: String -> Int
sumCaptcha [_]              = 0
sumCaptcha x@(x1 : x2 : xs) = case (x1 == x2) of
                                True  -> (digitToInt x1) + sumCaptcha (tail x)
                                False -> sumCaptcha (tail x)

sumCaptcha2 :: String -> Int -> Int
sumCaptcha2 x@(x1 : xs) hl = case (hl >= length x) of
                               True -> 0
                               False -> case (x1 == x !! hl) of
                                          True -> ((digitToInt x1) * 2) + sumCaptcha2 xs hl
                                          False -> sumCaptcha2 xs hl

main :: IO ()
main = do
    (s : _) <- getArgs
    -- print . sumCaptcha $ s ++ [head s]
    print $ sumCaptcha2 s ((length s) `div` 2)
