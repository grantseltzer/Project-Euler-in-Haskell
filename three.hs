-- What is the largest prime factor of the number 600851475143 ?

checkPrime :: (Integral a, Ord a, Num a) => [a] -> [a]
checkPrime (p:xs) = p : checkPrime [ x | x <- xs, x `mod` p > 0]

three = checkPrime $ [ z | z <- [2..600851475143], 600851475143 `mod` z == 0]
