
-- Problem 1
myLast :: [a] -> a
myLast (x:[]) = x 
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] =  error "out of bounds"
myButLast (x:[]) = error "out of bounds"
myButLast (x:_:[]) = x
myButLast (x:xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

-- Problem 4
myLength :: [a] -> Integer
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse (x:xs) = chg xs [x]
  where chg [] ret = ret
        chg (x:xs) ret = chg xs (x : ret)

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
--                    deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem e) = [e]
flatten (List l) = concat (map flatten l)

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = foldr (compress') [] xs
  where compress' x ys | (ys == []) = [x]
                       | (x == head ys)  = ys
                       | otherwise = x : ys 
