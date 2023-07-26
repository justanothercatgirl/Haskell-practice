import Data.List

path :: Int -> Int -> Int
path start end  
    | (start < end)      = 0
    | (start == 7)       = 0
    | (start == end)     = 1 
    | otherwise          = path (start-1) end + path (start-3) end + path (div start 2) end
    
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

allDivisors :: Int -> [Int]
allDivisors x = 
    concat [if d1 /= d2 then [d1, d2] else [d1]
        | d1 <- [1..n], 
        let d2 = div x d1, 
        mod x d1 == 0 ]
    where n = isqrt x + 1

has_n_divisors :: Int -> Int -> Bool
has_n_divisors x n = length (allDivisors x) == n

tryNumbersRange :: (Int, Int) -> Int -> [[Int]]
tryNumbersRange (low, upp) n = 
    [take 2 ( sortBy (flip compare) (allDivisors x) ) 
    | x <- [low..upp], has_n_divisors x n]

numbersMasked :: [Int]
numbersMasked = [123450708 + x + y | x <- [0, 1000..9999], y <- [1, 10..99]]

doDivisions :: Int -> [Int] -> [(Int, Int)]
doDivisions n list = [(x, div x n) | x <- list, mod x n == 0]

intToList :: Int -> [Int]
intToList x = [fromEnum y - 48 | y <- show x]

sumOddDigits :: Int -> Int
sumOddDigits x = sum [d | d <- (intToList x), odd d]

numMask1 = [12400579 + x | x <- [1000, 2000..99999]]
numMask2 = [12450079 + x | x <- [100, 200..9999]]
numMask3 = [12305079 + x + y | x <- [100, 200..999], y <- [10000, 20000..99999]]
allNumsMasked = numMask1 ++ numMask2 ++ numMask3

specificSort (a, _) (b, _) = compare a b

doMaskings xs = [(x, sum . intToList $ x) | x <- xs, mod x (sumOddDigits x) == 0]

main :: IO()
main = print(sortBy specificSort . doMaskings $ allNumsMasked)
