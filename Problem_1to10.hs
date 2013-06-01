myLast :: [a] -> a
myLast (x:[]) = x 
myLast (x:xs) = myLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

myLength :: [a] -> Integer
myLength []     = 0
myLength (x:xs) = 1 + myLength xs
