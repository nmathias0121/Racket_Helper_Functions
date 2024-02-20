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

-- return true if string contains character
contains :: Char -> [Char] -> Bool 
contains ch s | s == "" = False 
              | (head s) == ch = True 
              | otherwise = contains ch (tail s)

-- return true if all characters in string are unique
is_unique :: [Char] -> Bool
is_unique s | s == "" = True 
            | contains (head s) (tail s) == True = False 
            | contains (head s) (tail s) == False = True


-- uniquify string : remove repititve consecutive characters in a string
uniquify :: [Char] -> [Char]
uniquify s | s == "" = ""
           | tail s == "" = s       -- same as (take 1 s)
           | contains (head s) (tail s) == True = uniquify (tail s)
           | contains (head s) (tail s) == False = (take 1 s) ++ (uniquify (tail s))
