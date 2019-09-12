{- by pedrito8472
Given an array of integers, return a new array such that each element at index i 
of the new array is the product of all the numbers in the original array except 
the one at i.

For example, if our input was [1, 2, 3, 4, 5], the expected output would be 
[120, 60, 40, 30, 24]. If our input was [3, 2, 1], the expected output would be [2, 3, 6].

Follow-up: what if you can't use division?

My naive solution using divison works in O(2n) which is reasonable.                 
Considering the case where there might be a zero in the list is not trivial.

  -}
import Data.List
day2 :: Integral p => [p] -> [p]
day2 list = day2' list (product list) 

day2' :: Integral p => [p] -> p -> [p]
day2' list prod = map (\x -> prod `div` x) list

day2'' :: Integral p => [p] -> [p]
day2'' list = map (\x -> product (delete x list)) list



main = do
  print (day2'' [1,2,3,4,5])
