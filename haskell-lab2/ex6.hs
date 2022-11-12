{-# LANGUAGE BangPatterns #-}

fib :: (Num a, Eq a) => a -> a
fib n =
    if n == 0 || n == 1 then n
    else fib (n - 2) + fib (n - 1) -- O(2^n)
xs = [fib (x*5) | x <- [1..8]]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]
fib2 :: Int -> Int
fib2 n = fibs !! n -- O(n)
xss = [fib2 (x*10) | x <- [1..10]]

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs -- przepełnienie stosu przy liście [1..10000000]

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) = x == n || elem' n xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = 2*x : doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) = x*x : squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = if x `mod` 2 == 0
    then x : selectEven xs
    else selectEven xs

arithmeticAverage l = fromIntegral (sum l) / fromIntegral (length l)

geometricAverage l = fromIntegral (product l) ** (1/fromIntegral (length l))

average l = (arithmeticAverage l, geometricAverage l)

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
    where   loop acc []     = acc
            loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
    where   loop acc []     = acc
            loop acc (x:xs) = loop (x + acc) xs -- przepełnienie stosu przy liście [1..100000000]

prod'2 :: Num a => [a] -> a
prod'2 xs = loop 1 xs
    where   loop acc []     = acc
            loop acc (x:xs) = loop (x * acc) xs

length'2 :: [a] -> Int
length'2 xs = loop 0 xs
    where   loop acc []     = acc
            loop acc (x:xs) = loop (1 + acc) xs

sum'4 :: Num a => [a] -> a
sum'4 = loop 0
    where   loop !acc []     = acc
            loop !acc (x:xs) = loop (x + acc) xs -- czas wykonywania: sum < sum'4 < sum'3
