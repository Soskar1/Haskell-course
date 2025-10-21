-- Exercise 1
average :: [Float] -> Float
average array = arraySum array / (fromIntegral (length array))
    where
        arraySum (x:xs)
            | length xs == 0 = x
            | otherwise = x + arraySum xs