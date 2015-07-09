-- Find the sum of all the multiples of 3 or 5 below 1000.
one = do
        sum [ x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]

fib n
        | n == 1 = 1
        | n == 2 = 2
        | otherwise = fib (n-1) + fib (n-2)
        
-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, 
-- find the sum of the even-valued terms.

two = do
        sum [ y | y <- [1..4000000], fib y `mod` 2 == 0 ]

checkPrime :: (Integral a, Ord a, Num a) => [a] -> [a]
checkPrime (p:xs) = p : checkPrime [ x | x <- xs, x `mod` p > 0]

-- What is the largest prime factor of the number 600851475143 ?

three = do
        checkPrime $ [ z | z <- [2..600851475143], 600851475143 `mod` z == 0]
