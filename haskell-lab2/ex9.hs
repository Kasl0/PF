qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where
        leftPart  xs = [ y | y <- xs, y <= x ]
        rightPart xs = [ y | y <- xs, y > x  ]

qSort2 :: Ord a => [a] -> [a]
qSort2 []     = []
qSort2 (x:xs) = qSort2 (leftPart xs) ++ [x] ++ qSort2 (rightPart xs)
    where
        leftPart  xs = filter (<=x) xs
        rightPart xs = filter (>x) xs

concat'' :: [[a]] -> [a]
concat'' []       = []
concat'' (x:xs) = x ++ concat'' xs

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = if x <= head xs
    then isSorted xs
    else False

reverse' :: [a] -> [a]
reverse' [] = []
reverse' x = last [x] ++ reverse' (init x)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' (x:xs) (y:ys) = [(x, y)] ++ zip' xs ys

unzip' :: [(a, b)] -> ([a],[b])
unzip' [] = ([], [])
unzip' (x:xs) = ([fst x, fst (head xs)], [snd x, snd (head xs)])

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] [] [] = []
zip3' (x:xs) (y:ys) (z:zs)= [(x, y, z)] ++ zip3' xs ys zs

subList :: Eq a => [a] -> [a] -> Bool
subList s l = s `elem` sublists l
    where   sublists [] = [[]]
            sublists (x:xs) = [x:sublist | sublist <- sublists xs] ++ sublists xs
