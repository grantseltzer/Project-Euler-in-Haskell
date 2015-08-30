digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

fac :: (Num a, Eq a) => a -> a
fac 0 = 1
fac x = x * fac (x-1)

--Find the sum of the digits in the number 100!
twenty = do
    sum $ digs $ fac 100


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

--Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
twentyThree = do
        sum [ x | x <- [1..28123], not $ x `elem` nowAll abundants]
        where abundants = [ x | x <- [1..28123], abun x (factors x)]
