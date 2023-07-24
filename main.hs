import Data.List
 
 
prodRepeat xs 1 = [[a] | a <- xs]
prodRepeat xs y = [a:b | a <- xs, b <- prodRepeat xs (y-1)] 

prod2 xss = 
    if length xss == 2 
    then [a:b:[] | a <- head xss, b <- last xss] 
    else xss
prod xss = 
    if length xss > 2 
    then [a:b | a <- head xss, b <- prod (tail xss)] 
    else prod2 xss
 
f x y z w = (x <= (z==w)) || not (y <= w)
 
allTables = [ [[x1, True, x2, x3, False], 
               [False, x4, False, x5, False], 
               [x6, False, False, x7, False]] | 
    [x1, x2, x3, x4, x5, x6, x7] <- 
    prodRepeat [False, True] 7 ]
 
 
count x [] = 0
count x xs = if x == head xs then 1 + count x (tail xs) else count x (tail xs)
 
 
fWrapper xsss = [x1:x2:x3:x4:x5:[] | 
    xss <- xsss, [x1, x2, x3, x4, x5] <-xss, 
    f x1 x2 x3 x4 == x5 ]
 

unique [] = []
unique xs = if head xs `elem` tail xs then 
        unique (tail xs) 
        else 
        head xs : unique (tail xs)
 

lst = "EKMOpPNbU"

count' :: [[Char]] -> Integer -> Integer
count' xs accum = 
    if (count 'K' (head xs) == 2 && head (head xs) /= 'b' && mod accum 2 == 1)
    then accum
    else count' (tail xs) (accum + 1)

main :: IO ()
main = do
    print (9^5 + 1 - count' (reverse (prodRepeat lst 5)) 1) 
 
 
{-
    https://www.kompege.ru/task
-}
 
