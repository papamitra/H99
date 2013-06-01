myLast :: [a] -> a
myLast (x:[]) = x 
myLast (x:xs) = myLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)
