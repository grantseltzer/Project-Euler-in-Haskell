-- 2) By considering the terms in the Fibonacci sequence whose values do not exceed four million, 
-- find the sum of the even-valued terms.

fib n
        | n == 1 = 1
        | n == 2 = 2
        | otherwise = fib (n-1) + fib (n-2)
        
two = sum [ y | y <- [1..4000000], fib y `mod` 2 == 0 ]
