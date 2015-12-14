-- Takes an integer and returns a list of it's digits
digits :: Int -> [Int]
digits x
    | x == 0 = []
    | otherwise = x `mod` 10 : digits (x `div` 10)

-- Takes an integer and returns true if it's chain ends in 89
squareDigit :: Int -> Bool
squareDigit x
    | x == 1 = False
    | x == 89 = True
    | otherwise = squareDigit (sum(map (\x -> x*x) (digits x)))

-- Takes a list of Integers and returns the number of Integers in that list whose chains end in 89
chain :: [Int] -> Int
chain [] = 0
chain (x:xs)
    | squareDigit x == True = 1 + chain xs
    | otherwise = chain xs
