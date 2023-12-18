{-# LANGUAGE RankNTypes #-}
-- list_operations.hs
{-
Haskell List Operations
Developer Name : Neil Mathias     
Developer E-mail : neilmathias25@gmail.com
-}


-- first lst -> returns first element of the list (lst !! 1)
--lst is not empty
first :: [a] -> a
first lst = head (lst)

-- fifth lst -> returns fifth element of the list (lst !! 4)
--lst has at least five elements
fifth :: [a] -> a
fifth lst = head (take 1 (drop 4 lst))

-- get n lst -> returns nth element of the list (lst !! n)
--lst has at least n elements
-- example input : get 5 [1,2,3,4,5,6,7] => 5
get :: Int -> [a] -> a
get n lst = head (drop (n-1) lst)
