roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) =
 let d = sqrt (b * b - 4 * a * c)
     e = 2 * a
 in ( (-b - d) / e, (-b + d) / e )

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) =
    let r = sqrt(x * x + y * y)
    in (x/r, y/r)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x, y, z) =
    let r = sqrt(x * x + y * y + z * z)
    in (x/r, y/r, z/r)

triangle :: (Double, Double, Double) -> Double
triangle (a, b, c) =
    let p = (a + b + c)/2
    in sqrt (p * (p-a) * (p-b) * (p-c))