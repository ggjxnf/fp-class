-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms x = (x `div` 3600, (x `div` 60) `mod` 60 , x `mod` 60)

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

-- triangle :: ??? -> (Double, Double)
triangle :: Point -> Point -> Point -> (Double, Double)
triangle a b c = (p, s)
  where
    p = distance a b + distance a c + distance b c
    s = sqrt $ p / 2 * (p / 2 - distance a b) * (p / 2 - distance a c) * (p / 2 - distance b c)

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
	| even x = 1 + nEven xs
	| otherwise = nEven xs

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x : xs) = x * 2 : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs)
	| odd x = x : fltOdd xs
	| otherwise = fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
rmNeg :: Integral a => [a] -> [a]
rmNeg [] = []
rmNeg (x:xs)
	| x > 0 = x : rmNeg xs
	| otherwise = rmNeg xs

-- б) увеличить элементы с чётными значениями в два раза;
doubleEven :: Integral a => [a] -> [a]
doubleEven [] = []
doubleEven (x:xs)
	| even x = x * 2 : doubleEven xs
	| otherwise = x : doubleEven xs

-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
swapPairs :: Integral a => [a] -> [a]
swapPairs [] = []
swapPairs [x] = []
swapPairs (x:x1:xs) = x1 : x : swapPairs xs

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = x + y : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combinePairs :: [Integer] -> [Integer] -> [(Integer, Integer)]
combinePairs [] ys = []
combinePairs xs [] = []
combinePairs (x:xs) (y:ys) = (x, y) : combinePairs xs ys


-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
firstDesc :: Integer -> [Integer]
firstDesc 1 = [1]
firstDesc n
	| n <= 0 = error "n should be > 0"
	| otherwise = n : firstDesc (n - 1)

-- б) в порядке возрастания.
firstAsc :: Integer -> [Integer]
firstAsc 1 = [1]
firstAsc n
	| n <= 0 = error "n should be > 0"
	| otherwise = firstAsc (n - 1) ++ [n]

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
putBetween :: a -> [a] -> [a]
putBetween _ [] = []
putBetween y [x] = [x]
putBetween y (x : xs) = x : y : putBetween y xs

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
splitByFirst :: Eq a => [a] -> ([a], [a])
splitByFirst [] = ([], [])
splitByFirst (x : xs) = (takeWhile1 (==x) (x : xs), dropWhile1 (==x) (x : xs))
	where
		takeWhile1 :: (a -> Bool) -> [a] -> [a]
		takeWhile1 _ [] = []
		takeWhile1 p (y : ys)
			| p y = y : takeWhile1 p ys
			| otherwise = []
		dropWhile1 :: (a -> Bool) -> [a] -> [a]
		dropWhile1 _ [] = []
		dropWhile1 p (x : xs)
			| p x = dropWhile1 p xs
			| otherwise = (x : xs)


--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
elemByIndex :: [a] -> Int -> a
elemByIndex [] _ = error "index out of bounds"
elemByIndex (x : xs) i
	| i < 0 = error "index should be >= 0"
	| i == 0 = x
	| otherwise = elemByIndex xs (i - 1)

-- б) Eq a => [a] -> a -> Bool
contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x : xs) a
	| x == a = True
	| otherwise = contains xs a

-- в) [a] -> Int -> [a]
takeN :: [a] -> Int -> [a]
takeN [] _ = []
takeN (x : xs) n
	| n < 0 = error "n should be >= 0"
	| n == 0 = []
	| otherwise = x : takeN xs (n - 1)

-- г) a -> Int -> [a]
replicateN :: a -> Int -> [a]
replicateN x n
	| n < 0 = error "n should be >= 0"
	| n == 0 = []
	| otherwise = x : replicateN x (n - 1)

-- д) [a] -> [a] -> [a]
combine :: [a] -> [a] -> [a]
combine [] ys = ys
combine xs [] = xs
combine (x : xs) ys
	| null xs = x : ys
	| otherwise = x : combine xs ys

-- е) Eq a => [a] -> [[a]]
group1 :: Eq a => [a] -> [[a]]
group1 [] = [[]]
group1 [x] = [[x]]
group1 (x : xs) = iter xs [x] []
	where
		iter [] [] result = result
		iter [] current result = result ++ [current]
		iter (x : xs) current result
			| x == head current = iter xs (x : current) result
			| otherwise = iter xs [x] (result ++ [current])

-- ж) [a] -> [(Int, a)]
addIndexes :: [a] -> [(Int, a)]
addIndexes [] = []
addIndexes xs = iter xs 0
	where
		iter [] _ = []
		iter (x : xs) i = (i, x) : iter xs (i + 1)

-- з) Eq a => [a] -> [a]
distinct :: Eq a => [a] -> [a]
distinct xs = iter $ group1 xs
	where
		iter :: [[a]] -> [a]
		iter [] = []
		iter (x : xs) = head x : iter xs
