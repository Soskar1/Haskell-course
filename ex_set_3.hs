-- Exercise 1
type Position = (Float, Float)
xPos :: Position -> Float
xPos (x, _) = x

yPos :: Position -> Float
yPos (_, y) = y

data Shape = Circle Float Position | Rectangle Float Float Position
    deriving (Show, Ord, Eq)

overlaps :: Shape -> Shape -> Bool
overlaps (Rectangle w1 h1 pos1) (Rectangle w2 h2 pos2) =
    let x1 = xPos pos1
        y1 = yPos pos1
        x2 = xPos pos2
        y2 = yPos pos2
        x1Left = x1 - w1 / 2
        x1Right = x1 + w1 / 2
        y1Top = y1 + h1 / 2
        y1Bottom = y1 - h1 / 2
        x2Left = x2 - w2 / 2
        x2Right = x2 + w2 / 2
        y2Top = y2 + h2 / 2
        y2Bottom = y2 - h2 / 2
    in
        (x1Left < x2Right) && (x1Right > x2Left) && (y1Top > y2Bottom) && (y1Bottom < y2Top)

overlaps (Circle r1 pos1) (Circle r2 pos2) = (diff <= distance) && (distance <= (r1 + r2))
    where
        diff = abs(r1 - r2)
        distance = sqrt(((xPos pos1) - (xPos pos2))**2 + ((yPos pos1) - (yPos pos2))**2)

overlaps (Rectangle w h pos1) (Circle r pos2) =
    let
        xRect = xPos pos1
        yRect = yPos pos1

        xLeft = xRect - w / 2
        xRight = xRect + w / 2
        yTop = yRect + h / 2
        yBottom = yRect - h / 2

        xCircle = xPos pos2
        yCircle = yPos pos2

        xNearest = max xLeft (min xCircle xRight)
        yNearest = max yBottom (min yCircle yTop)

        distanceX = xNearest - xCircle
        distanceY = yNearest - yCircle
    in
        (distanceX**2 + distanceY**2) <= r**2

overlaps (Circle r pos1) (Rectangle w h pos2) = overlaps (Rectangle w h pos2) (Circle r pos1)

-- Exercise 2
any1 :: (a -> Bool) -> [a] -> Bool
any1 cond arr = not (null (filter cond arr))

any2 :: (a -> Bool) -> [a] -> Bool
any2 cond arr = foldr (||) False (map cond arr)

all1 :: (a -> Bool) -> [a] -> Bool
all1 cond arr = length (filter cond arr) == length arr

all2 :: (a -> Bool) -> [a] -> Bool
all2 cond arr = foldr (&&) True (map cond arr)

-- Exercise 3
unzipping :: (a, b) -> ([a], [b]) -> ([a], [b])
unzipping (a, b) (as, bs) = (a : as, b : bs)

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip arr = foldr unzipping ([], []) arr

-- Exercise 4
myLength1 :: [a] -> Int
myLength1 arr = (sum . map (\_ -> 1)) arr

myLength2 :: [a] -> Int
myLength2 arr = foldr addOne 0 arr
    where
        addOne _ num = num + 1

-- Exercise 5
ff :: Integer -> [Integer] -> Integer
ff maxNum arr = (last . takeWhile (<= maxNum) . scanl1 (+) . map (*10) . filter (>0)) arr

-- Exercise 6
total :: (Integer -> Integer) -> Integer -> Integer
total f n = (sum . map f) [0..n]

-- Exercise 7
iter1 :: Integer -> (a -> a) -> (a -> a)
iter1 n f
    | n <= 0 = id
    | otherwise = f . iter1 (n - 1) f

iter2 :: Integer -> (a -> a) -> (a -> a)
iter2 n f
    | n <= 0 = id
    | otherwise = foldr (.) id (replicate (fromIntegral n) f)

-- Exercise 8
splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (x:xs) = scanl split ([], (x:xs)) (x:xs)
    where
        split (xs, y:ys) z = (xs ++ [z], ys)