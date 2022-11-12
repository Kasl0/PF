fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fstDivScd :: Integral a => [a] -> Bool
fstDivScd (x : y : _) | y `mod` x == 0 = True
fstDivScd _ = False

fstDivThr :: Integral a => [a] -> Bool
fstDivThr (x : y : z : _) | z `mod` x == 0 = True
fstDivThr _ = False
