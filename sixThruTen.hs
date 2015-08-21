--Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
six = do
    (sum [1..100])^2 - (sum $ map (^2) [1..100])

primes :: (Integral a, Ord a, Num a) => [a] -> [a]
primes (p:xs) = p : primes [ x | x <- xs, x `mod` p > 0]

--What is the 10 001st prime number?
seven = do
    primes [2..] !! 10000
    

tupleMult (x,y,z) = x * y * z

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
nine = do 
    tupleMult $ head [ (x,y,z) | x <- [1..1000], y <- [1..1000], z <- [1..1000], x < y, y < z, x^2+y^2==z^2, x+y+z==1000]
