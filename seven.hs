--What is the 10 001st prime number?

primes :: (Integral a, Ord a, Num a) => [a] -> [a]
primes (p:xs) = p : primes [ x | x <- xs, x `mod` p > 0]

seven = primes [2..] !! 10000
