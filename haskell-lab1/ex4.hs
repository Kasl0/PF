sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x,y,z) = sqrt (x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (n, c) = (c, n)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = x == y && x == z

triangle :: (Double, Double, Double) -> Double
triangle (a, b, c) = sqrt ((a + b + c)/2 * ((a + b + c)/2-a) * ((a + b + c)/2-b) * ((a + b + c)/2-c))
