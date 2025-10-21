-- Exercise 1
nAnd1 :: Bool -> Bool -> Bool
nAnd1 True x
    | x == True = False
    | otherwise = True
nAnd1 False x = True

nAnd2 :: Bool -> Bool -> Bool
nAnd2 x y = not (x && y)

nAnd3 :: Bool -> Bool -> Bool
nAnd3 True True = False
nAnd3 True False = True
nAnd3 False True = True
nAnd3 False False = True

-- Exercise 2
nDigits :: Integer -> Int
nDigits x
    | x < 0 = length (show x) - 1
    | otherwise = length (show x)

-- Exercise 3
nRoots :: Float -> Float -> Float -> Int
nRoots a b c
    | a == 0 = error "the first argument shouls be non-zero!"
    | b^2 > 4 * a * c = 2
    | b^2 == 4 * a * c = 1
    | b^2 < 4 * a * c = 0

-- Exercise 4
discriminant_sqrt :: Float -> Float -> Float -> Float
discriminant_sqrt a b c = sqrt(b^2 - 4 * a * c)

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c =
    if nRoots a b c > 0
        then (-b - discriminant_sqrt a b c) / (2 * a)
        else error "The math equation with the provided coefficients a, b, and c does not have any roots"

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c =
    if nRoots a b c > 0
        then (-b + discriminant_sqrt a b c) / (2 * a)
        else error "The math equation with the provided coefficients a, b, and c does not have any roots"

-- Exercise 5
power2 :: Integer -> Integer
power2 n
    | n < 0 = 0
    | n == 0 = 1
    | n > 0 = power2 (n - 1) * 2

-- Exercise 6
mult :: Integer -> Integer -> Integer
mult m n
    | m == 0 = 0
    | m < 0 = mult (m + 1) n - n
    | m > 0 = mult (m - 1) n + n

-- Exercise 7
prod :: Integer -> Integer -> Integer
prod m n
    | m > n = error "first argument must be smaller than the second one!"
    | m == n = n
    | m < n = m * prod (m + 1) n