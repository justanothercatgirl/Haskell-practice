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
import Prelude

--read file
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

--max length
max' ls1 ls2
    | length ls1 < length ls2 = ls2
    | otherwise = ls1


--biggest list of divisors 
bgdvlst [x] = allDivisors x
bgdvlst (x:ls) = max' (allDivisors x) (bgdvlst ls)

--common elements
comels ls1 ls2 = [y | y <- ls1, y `elem` ls1 && y `elem` ls2] 

--checks whether a pair has an appropriate amount of common divisors
driver d1 d2 bgam = 
    length (comels d1 bgam) >= 3 && 
    length (comels d2 bgam) >= 3


--processes the data so the task actually works
process :: [Int] -> (Int, Int)
process whole@(x:ls) = (length ans, maximum ans)
    where
        ans = [length (comels x' y') |   
            (x, y) <- zip whole ls, 
            let x' = allDivisors x,
            let y' = allDivisors y,
            driver x' y' z']
        z' = bgdvlst whole

main = do
        handle <- openFile "a.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        let list = f singlewords
        print (bgdvlst list)
        print (process list)
        hClose handle
        
    
