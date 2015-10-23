-- 14) Which starting number, under one million, produces the longest collatz sequence?
condition :: Int -> Int
condition x
    | x `mod` 2 == 0 = x `div` 2
    | otherwise = 3*x+1

conList :: Int -> Int
conList n
    | n == 1 = []
    | otherwise = condition(n) : conList(condition(n)))


numToList :: [Int] -> [(Int, Int)]
numToList [] = []
numToList (x:xs) = (x, length(x:conList(x))):numToList(xs)

maxTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxTuple (a,b) (c,d)
    | b > d = (a,b)
    | d > b = (c,d)
    | otherwise = (a,b)

main :: IO ()
main = print (foldl maxTuple (1,1) (numToList [1..1000000]))
