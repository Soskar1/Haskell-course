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