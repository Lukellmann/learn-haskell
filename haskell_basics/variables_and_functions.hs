double x    = x * 2
quadruple x = double (double x)
square x    = x * x
half x      = x / 2
hm12 x      = (half x) - 12

r = 5.0 -- radius of a circle
{--
areaCircle = pi * r ^ 2 -- area of a circle
--}
areaCircle radius        = pi * radius ^ 2
areaRect length widht    = length * widht
areaSquare side          = areaRect side side
areaTriangle base height = (base * height) / 2

volumeBox length widht height = length * widht * height
volumeCylinder radius height  = (areaCircle radius) * height


areaTriangleTrig a b c  = c * height / 2
    where
    cosa   = (b ^ 2 + c ^ 2 - a ^ 2) / (2 * b * c)
    sina   = sqrt (1 - cosa ^ 2)
    height = b * sina
areaTriangleHeron a b c = result
    where
    result = sqrt (s * (s - a) * (s - b) * (s - c))
    s      = (a + b + c) / 2
