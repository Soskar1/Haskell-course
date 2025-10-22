-- Exercise 1
average :: [Float] -> Float
average [] = 0
average array = arraySum array / (fromIntegral (length array))
    where
        arraySum [] = 0
        arraySum (x:xs) = x + arraySum xs

-- Exercise 2
divides :: Integer -> [Integer]
divides num
    | num <= 0 = []
    | otherwise = getDivisors num []
    where
        isDivisor n currentNum = mod n currentNum == 0
        getDivisors currentNum arr
            | currentNum <= 0 = arr
            | isDivisor num currentNum = getDivisors (currentNum - 1) (currentNum : arr)
            | otherwise = getDivisors (currentNum - 1) arr