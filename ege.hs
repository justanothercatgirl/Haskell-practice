prodRepeat xs 1 = [[a] | a <- xs]
prodRepeat xs y = [a:b | a <- xs, b <- prodRepeat xs (y-1)]

count x [] = 0
count x xs = if x == head xs then 1 + count x (tail xs) else count x (tail xs)

count' :: [[Char]] -> Integer -> Integer
count' xs accum = 
    if (all [count c (head xs) == 1 | c <- head xs] && )
    then accum
    else count' (tail xs) (accum + 1)

lst = "0123456789ABCDEF"

digToDec 'x' = 
digToDec x = (fromEnum x - fromEnum '0')

toDec :: [Char] -> Int -> Integer
toDec num base = 

main :: IO()
main = do
    print (9^5 + 1 - count' (reverse (prodRepeat lst 5)) 1) 
