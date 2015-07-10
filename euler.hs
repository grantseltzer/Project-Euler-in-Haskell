-- 1) Find the sum of all the multiples of 3 or 5 below 1000.
one = do
        sum [ x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]
        
-- fib is used in two, recursivly finds the fibonacci sequence
fib n
        | n == 1 = 1
        | n == 2 = 2
        | otherwise = fib (n-1) + fib (n-2)
        
-- 2) By considering the terms in the Fibonacci sequence whose values do not exceed four million, 
-- find the sum of the even-valued terms.

two = do
        sum [ y | y <- [1..4000000], fib y `mod` 2 == 0 ]

checkPrime :: (Integral a, Ord a, Num a) => [a] -> [a]
checkPrime (p:xs) = p : checkPrime [ x | x <- xs, x `mod` p > 0]

-- 3) What is the largest prime factor of the number 600851475143 ?

three = do
        checkPrime $ [ z | z <- [2..600851475143], 600851475143 `mod` z == 0]
    
-- mog is a function that returns a list of a number, mod every number in a list. It's used in five
-- to make the solution a tad bit more elequent.

-- Find the largest palindrome made from the product of two 3-digit numbers.
four = do
        maximum [ x | y <- [100..999], z <- [y..999], let x=y*z, let s = show x, s==reverse s]


mog :: (Integral a) => a -> [a] -> [a]
mog _ [] = []
mog n (r:rs) = (n `mod` r) : mog n rs

-- 5) What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
five = do
        head [ x | x <- [1..], (sum $ mog x [1..20]) == 0]

