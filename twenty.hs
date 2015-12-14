--Find the sum of the digits in the number 100!

-- Take a number
digs :: Int -> [Int]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

fac :: Int -> Int
fac 0 = 1
fac x = x * fac (x-1)

twenty = sum $ digs $ fac 100
