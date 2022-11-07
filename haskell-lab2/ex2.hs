fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_ToPower5 :: Num a => a -> a
_ToPower5 = (^ 5)

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 -)

subtr5From_ :: Num a => a -> a
subtr5From_ = subtract 5

flip2 :: Num a => (a -> b -> c) -> b -> a -> c
flip2 f x y = f y x

flip3 :: Num a => (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f x y z = f z y x
