-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys n | n <= 1 = (x:ys)
                | otherwise = insertAt' x ys n []
  where insertAt' x [] _ ret = (reverse ret) ++ [x]
        insertAt' x ys 1 ret = (reverse ret) ++ [x] ++ ys
        insertAt' x (y:ys) n ret = insertAt' x ys (n-1) (y:ret)

-- Problem 22
range :: Integer -> Integer -> [Integer]
range lb rb = [lb..rb]


