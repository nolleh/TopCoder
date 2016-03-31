solve :: [Int] -> Int
solve [] = 0
solve list@(x:xs)
  | next == -1 = -1
  | (length get == x) && (same get) = 1 + next
  | otherwise = -1
  where get = take x list
        next = solve (drop x list)

same :: [Int] -> Bool
same (x:[]) = True
same (x:y:xs)
  | x /= y = False
  | otherwise = same (y:xs)

main = do
  print $ solve [2,2,3,3,3] -- 2
  print $ solve [2,2,2,2,3,3,3] -- 3
  print $ solve [1,1,1,1,1] -- 5
  print $ solve [3,3] -- -1
  print $ solve [4,4,4,4,1,1,2,2,3,3,3] -- 5
  print $ solve [2,1,2,2,1,2] -- -1
  print $ solve [2,2,3,3] -- -1
  print $ solve [2,2,2,2,4,4,4,4,3,3,2,2] -- -1