--Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

factors n = [ x | x <- [1..(n-1)], n `mod` x == 0]

abun :: (Num a, Ord a) => a -> [a] -> Bool
abun n xs
        | sum (xs) > n = True
        | otherwise     = False

mult :: (Num a) => [a] -> [a]
mult [g] = []
mult (x:w:xs) = (x + w) : mult (x:xs)

helper :: (Num a) => [a] -> [a]
helper (x:xs) = (x+x) : mult (x:xs)

nowAll :: (Num a) => [a] -> [a]
nowAll [] = []
nowAll (w:wx) = helper (w:wx) ++ nowAll (wx)

twentyThree = sum [ x | x <- [1..28123], not $ x `elem` nowAll abundants]
              where abundants = [ x | x <- [1..28123], abun x (factors x)]
