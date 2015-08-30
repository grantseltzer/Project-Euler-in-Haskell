digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

fac :: (Num a, Eq a) => a -> a
fac 0 = 1
fac x = x * fac (x-1)

--Find the sum of the digits in the number 100!
twenty = do
    sum $ digs $ fac 100
