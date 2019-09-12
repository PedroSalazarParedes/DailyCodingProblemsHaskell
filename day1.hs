{- by pedrito8472
Given a list of numbers and a number k, return whether any two numbers from the list add up to k.

For example, given [10, 15, 3, 7] and k of 17, return true since 10 + 7 is 17.

Bonus: Can you do this in one pass?

My idea for a solution (imperative :( ):
1. posible answers are all possible k-L[i]
2. For each item i in the list, check if it's a possible solution. If it's not, calculate k-i and store the in a list of candidates. If it is, end.
-}

day1 :: Integral p => [p] -> p -> Bool -- Should try to think of a way to make this Eq and Num, not Integral
day1 list k = day1' list k []

day1' :: Integral p => [p] -> p -> [p] -> Bool -- Candidates should probably be a hash set
day1' [] k candidates = False
day1' (x:xs) k [] = day1' xs k [k-x]
day1' (x:xs) k candidates
  | elem x candidates = True
  | otherwise = day1' xs k (k-x:candidates)


main = do
  print (day1 [10, 15, 2, 35] 17)
