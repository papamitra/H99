-- Problem 14
dupli :: [a] -> [a]
dupli xs = reverse (foldl (\x y -> y:y:x) [] xs)
