-- Exercise 1
average :: [Float] -> Float
average [] = 0
average array = arraySum array / (fromIntegral (length array))
    where
        arraySum [] = 0
        arraySum (x:xs) = x + arraySum xs

-- Exercise 2
isDivisor :: Integer -> Integer -> Bool
isDivisor num1 num2 = mod num1 num2 == 0

divides :: Integer -> [Integer]
divides num
    | num <= 0 = []
    | otherwise = getDivisors num []
    where
        getDivisors currentNum arr
            | currentNum <= 0 = arr
            | isDivisor num currentNum = getDivisors (currentNum - 1) (currentNum : arr)
            | otherwise = getDivisors (currentNum - 1) arr

divides2 :: Integer -> [Integer]
divides2 num
    | num <= 0 = []
    | otherwise = [n | n <- [1..num], isDivisor num n]

isPrime :: Integer -> (Integer -> [Integer]) -> Bool
isPrime num getDivisors
    | num <= 0 = False
    | otherwise = length divisors == 2 && head divisors == 1 && last divisors == num
        where
            divisors = getDivisors num

-- Exercise 3
prefix :: String -> String -> Bool
prefix str1 str2
    | length str1 == 0 = True
    | length str1 > length str2 = False
    | otherwise = strCompare str1 str2
        where
            strCompare [] _ = True
            strCompare (x:xs) (y:ys)
                | x == y = strCompare xs ys
                | otherwise = False

substring :: String -> String -> Bool
substring str1 str2
    | length str1 == 0 = True
    | length str2 == 0 = False
    | length str1 > length str2 = False
    | prefix str1 str2 = True
    | otherwise = substring str1 (tail str2)