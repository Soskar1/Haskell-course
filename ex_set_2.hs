import Data.Char

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
substring [] _ = True
substring _ [] = False
substring str1 str2
    | prefix str1 str2 = True
    | otherwise = substring str1 (tail str2)

-- Exercise 4
permut :: [Integer] -> [Integer] -> Bool
permut [] [] = True
permut [] _ = False
permut _ [] = False
permut (x:xs) (y:ys)
    | removeResult == (y:ys) = False
    | otherwise = permut xs removeResult
        where
            remove _ [] = []
            remove value (x:xs)
                | value == x = xs
                | otherwise = x : remove value xs

            removeResult = remove x (y:ys)

-- Exercise 5
capitalise :: String -> String
capitalise str = [toUpper c | c <- str, isDigit c == False]

-- Exercise 6
type ShopItem = (String, Float)
type ShopBasket = [ShopItem]

name :: ShopItem -> String
name (n, _) = n

price :: ShopItem -> Float
price (_, p) = p

itemTotal :: ShopBasket -> ShopBasket
itemTotal [] = []
itemTotal basket = merge [] basket
    where
        getDuplicates _ [] = []
        getDuplicates item basket = [i | i <- basket, name i == name item]

        getPriceSum [] = 0
        getPriceSum (item:basket) = price item + getPriceSum basket
        
        contains _ [] = False
        contains item (itemInBasket:basket)
            | name item == name itemInBasket = True
            | otherwise = contains item basket

        merge currentBasket (item:basket)
            | contains item currentBasket = merge currentBasket basket
            | length basket == 0 = item:currentBasket
            | otherwise = merge ((name item, getPriceSum(getDuplicates item (item:basket))):currentBasket) basket