-- Problem 14
dupli :: [a] -> [a]
dupli xs = reverse (foldl (\x y -> y:y:x) [] xs)

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n = foldr (myRepeat n) [] xs
  where myRepeat 0 x ret = ret
        myRepeat n x ret = myRepeat (n-1) x (x:ret)

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = reverse $ dropEvery' [] xs (n-1)
  where dropEvery' xs [] _ = xs
        dropEvery' xs (y:ys) 0 = dropEvery' xs ys (n-1)
        dropEvery' xs (y:ys) n' = dropEvery' (y:xs) ys (n'-1)

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = ((take n xs), (drop n xs))

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs l r = headRemove (l-1) $ take r xs
  where headRemove n str@(x:xs) | n <= 0 = str
                                | otherwise =  headRemove (n-1) xs

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n | (n < 0) = foldr (:) (take (l+n) xs) (drop (l+n) xs)
            | (n == 0) = xs
            | (n > 0) = foldr (:) (take n xs) (drop n xs)
  where l = length xs


