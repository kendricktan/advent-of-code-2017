import           Data.Char
import           System.Environment

sumCaptcha :: String -> Int
sumCaptcha [_]              = 0
sumCaptcha x@(x1 : x2 : xs) = case (x1 == x2) of
                                True  -> (digitToInt x1) + sumCaptcha (tail x)
                                False -> sumCaptcha (tail x)

main :: IO ()
main = do
    (s : _) <- getArgs
    print . sumCaptcha $ s ++ [head s]
