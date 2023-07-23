import Data.List

prodRepeat xs y = 
    if y > 1 
    then [a:b | a <- xs, b <- prodRepeat xs (y-1)] 
    else [[a] | a <- xs]
 
prod2 xss = 
    if length xss == 2 
    then [a:b:[] | a <- head xss, b <- last xss] 
    else xss
prod xss = 
    if length xss > 2 
    then [a:b | a <- head xss, b <- prod (tail xss)] 
    else prod2 xss

f :: Bool a => a -> a -> a -> a -> a
f x y z w = (x <= (z==w)) || not (y <= w)

allTables = [ [[x1, True, x2, x3, False], 
               [False, x4, False, x5, False], 
               [x6, False, False, x7, False]] | [x1, x2, x3, x4, x5, x6, x7] <- prodRepeat [False, True] 7 ]

fWrapper xsss = [[x1, x2, x3, x4, x5] | [[[x1, x2, x3, x4, x5]]] <- allTables, f x1 x2 x3 x4 == x5]

main :: IO ()
main = do
print(test)


{-
    print([pow 2 x | x <- [0..20], mod x 3 /= 2])
    print( [x*y | x <- [1..4], y <- [2, 4..8], x*y >= 10] )
-}


