-- There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
nine = (\(x,y,z) -> x*y*z) $ head [ (x,y,z) | 
                  x <- [1..1000], y <- [1..1000], z <- [1..1000],
                                x < y, y < z, x^2+y^2==z^2, x+y+z==1000]
