-- Problem 14
dupli :: [a] -> [a]
dupli xs = reverse (foldl (\x y -> y:y:x) [] xs)

-- Problem 15
repli :: String -> Int -> String
repli str n = foldr (myRepeat n) [] str
  where myRepeat 0 c ret = ret
        myRepeat n c ret = myRepeat (n-1) c (c:ret)

-- Problem 16
dropEvery :: String -> Int -> String
dropEvery xs n = reverse $ dropEvery' xs n 1 ""
  where dropEvery' [] n i ret = ret
        dropEvery' (x:xs) n i ret | ((i `mod` n) == 0) = dropEvery' xs n 1 ret
                                  | otherwise = dropEvery' xs n (i+1) (x:ret) 

