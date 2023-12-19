{-# LANGUAGE RankNTypes #-}
-- list_operations.hs
{-
Haskell List Operations
Developer Name : Neil Mathias     
Developer E-mail : neilmathias25@gmail.com
-}

-- Calculate the size of the list
-- list can contain element of only one type
listSize :: [a] -> Int
listSize lst = length lst

-- first lst -> returns first element of the list (lst !! 1)
first :: [a] -> a
first lst | listSize lst == 0 = error "list is empty"
          | otherwise = head (lst)

-- fifth lst -> returns fifth element of the list (lst !! 4)
fifth :: [a] -> a
fifth lst | listSize lst < 5 = error "there is no 5th element"
          | otherwise = head (take 1 (drop 4 lst))

-- get n lst -> returns nth element of the list (lst !! n)
-- example input : get 5 [1,2,3,4,5,6,7] => 5
get :: Int -> [a] -> a
get n lst | listSize lst < n = error "there are not enough elements in this list"
          | otherwise = head (drop (n-1) lst)

-- Find the minimum value of a list
min_list :: [Int] -> Int
min_list xs = minimum xs

-- Find the maximum value of a list
max_list :: [Int] -> Int
max_list xs = maximum xs

-- Exception if b or h non-positive
triangle_area :: (Fractional a, Eq a, Ord a) => a -> a -> a
triangle_area b h | b <= 0  = error "triangle_area: b <= 0"
                  | h <= 0 = error "triangle_area: h <= 0"
                  | otherwise = (b * h) / 2

-- Exception if a, b or c non-positive
heron_area :: (Fractional a, Eq a, Floating a, Ord a) => a -> a -> a -> a
heron_area a b c | a <= 0  = error "heron_area: a must be positive"
                 | b <= 0 = error "heron_area: b must be positive"
                 | c <= 0 = error "heron_area: c must be positive"
                 | value < 0 = error "heron_area: not a triangle"
                 | otherwise = sqrt value / 4
                        where
                            value = (a + b + c) * (c - (a - b)) * (c + (a - b)) * (a + (b - c))
