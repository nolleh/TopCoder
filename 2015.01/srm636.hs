-- SRM 636
--main = print $  fromIntegral ( sum $ take 4 [7, 15, 9, 5]) / 4
main = do 
  print $ game_of_stones [7, 15, 9, 5]              --  3
  print $ game_of_stones [10, 16]                   -- -1 
  print $ game_of_stones [2, 8, 4]                  -- -1
  print $ game_of_stones [17]                       --  0
  print $ game_of_stones [10, 15, 20, 12, 1, 20]    -- -1

game_of_stones :: [Int] -> Int
game_of_stones xs = 
  iter xs 0 0 (length xs)
  where 
    iter xs spare result count
      | (null xs && spare == 0) = result
      | (null xs || count <= 0) = -1
      | head xs > avg = iter (tail xs) (spare + (head xs - avg)) result count
      | (head xs < avg && (head xs + 2 <= avg) && spare >= 2) = 
          iter ([(head xs)+2]++(tail xs)) (spare-2) (result+1) count
      | head xs == avg = iter (tail xs) spare result count
      | spare < 2 = iter ((tail xs)++[head xs]) spare result (count-1)
      | otherwise = -1 
    avg = div (sum xs) (length xs)
