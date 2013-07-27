-- Problem 14
dupli :: [a] -> [a]
dupli xs = reverse (foldl (\x y -> y:y:x) [] xs)

-- Problem 16
dropEvery :: String -> Int -> String
dropEvery xs n = reverse $ dropEvery' xs n 1 ""
  where dropEvery' [] n i ret = ret
        dropEvery' (x:xs) n i ret | ((i `mod` n) == 0) = dropEvery' xs n 1 ret
                                  | otherwise = dropEvery' xs n (i+1) (x:ret) 

