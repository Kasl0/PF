x = length [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a ^ 2 + b ^ 2 == c ^ 2]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == [] --nie, bo dla 1 i 0 zwraca True

howManyPrimes = length [i | i <- [2..10000], isPrime i]

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]
isPrime2 n = n `elem` take n primes --wolniejsza

howManyPrimesF n = length [i | i <- [1..n], isPrime2 i]

allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) xs
