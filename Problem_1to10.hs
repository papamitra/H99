myLast :: [a] -> a
myLast (x:[]) = x 
myLast (x:xs) = myLast xs

-- mindmind wrote
myButLast :: [a] -> a
myButLast [] =  error "out of bounds"
myButLast (x:[]) = error "out of bounds"
myButLast (x:_:[]) = x
myButLast (x:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

