{-
№ 4329 Пробный 06.2022 /dev/inf advanced (Уровень: Сложный)
В файле содержится последовательность целых чисел. 
Элементы последовательности могут принимать целые 
значения от 1 до 1000 включительно. Определите количество 
пар последовательности, в которых оба числа имеют хотя бы
3 общих делителя с числом последовательности, имеющим максимальное 
количество делителей. В ответе запишите количество найденных пар, 
затем максимальное количество общих делителей между элементами таких пар.
В данной задаче под парой подразумевается два идущих подряд 
элемента последовательности.

Файлы к заданию:17.txtПоказать ответ
Разбор

643 16
-}

import System.IO  
import Control.Monad
import Data.List

f :: [String] -> [Int]
f = map read 

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

allDivisors :: Int -> [Int]
allDivisors x = 
    concat [if d1 /= d2 then [d1, d2] else [d1]
        | d1 <- [1..n], 
        let d2 = div x d1, 
        mod x d1 == 0 ]
    where n = isqrt x + 1
    
max' ls1 ls2
    | length ls1 < length ls2 = ls2
    | otherwise = ls1

bgdvlst [x] = allDivisors x
bgdvlst (x:ls) = max' (allDivisors x) (bgdvlst ls)

hasxcom ls1 ls2 x = length [y | y <- ls1, y `elem` ls1 && y `elem` ls2] >= x

pp n1 n2 ngam = 
    hasxcom d1 ngam 3 && hasxcom d2 ngam 3
    where d1 = allDivisors n1 
          d2 = allDivisors n2

process ls = 


main = do  
        let list = []
        handle <- openFile "a.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list = f singlewords
        print (pp 6 6 [1, 2, 3, 4, 6, 12])
        hClose handle   
