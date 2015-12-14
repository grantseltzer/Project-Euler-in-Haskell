-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
five = head [ x | x <- [1..], (sum (map (x `mod`) [1..20])) == 0]
