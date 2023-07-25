import Prelude
import Control.Monad 
import System.Exit
 
mean :: Integral a => a -> a -> a
mean x y = div (x + y) 2


driver :: Integral a => a -> a -> a -> Ordering -> (a, a, a)
driver lower upper guess ord
    | lower == upper || ord == EQ = (lower, lower, lower)
    | ord == GT = (guess+1, upper, mean (guess+1) upper)
    | ord == LT = (lower, guess, mean lower guess)


process :: [Char] -> Ordering
process [] = GT
process (x:xs)
    | (x == 'Y' || x == 'y') = GT
    | (x == 'n' || x == 'N') = LT
    | otherwise = EQ;


loop :: Int -> Int -> Int -> IO()
loop l u g = do 
    putStr "is your number greater than "
    putStr . show $ g
    putStrLn "? [Y/n/<any key if equal>]"
    x0 <- getLine
    let (lower, upper, guess) = driver l u g (process x0) 
    when (lower /= upper) (loop lower upper guess)
    putStr "your number is "
    putStr . show $ g
    putStrLn "!!!"
    exitSuccess


main :: IO()
main = do
    loop 0 100 50
