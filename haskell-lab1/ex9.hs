roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where d = sqrt (b * b - 4 * a * c)
         e = 2 * a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = (x/r, y/r)
    where r = sqrt(x * x + y * y)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x, y, z) = (x/r, y/r, z/r)
    where r = sqrt(x * x + y * y + z * z)

triangle :: (Double, Double, Double) -> Double
triangle (a, b, c) = sqrt (p * (p-a) * (p-b) * (p-c))
    where p = (a + b + c)/2
