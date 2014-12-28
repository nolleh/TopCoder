-- SRM 636
--main = print $  fromIntegral ( sum $ take 4 [7, 15, 9, 5]) / 4
main = do 
  print $ game_of_stones [7, 15, 9, 5]              --  3
  print $ game_of_stones [10, 16]                   -- -1 
  print $ game_of_stones [2, 8, 4]                  -- -1
  print $ game_of_stones [17]                       --  0
  print $ game_of_stones [10, 15, 20, 12, 1, 20]    -- -1

game_of_stones :: [Int] -> Int
game_of_stones [] = 0
game_of_stones [x] = 0
game_of_stones (x:xs)
  | fst average == 0 = 0
  | snd average == False = -1
  | (possible smaller && possible larger) = optimize smaller (sum larger) 0
  | otherwise = -1
  	where 
      average = avg $ [x]++xs
      smaller = [a - fst average | a <- [x]++xs, a < fst average]
      larger = [a - fst average | a <- [x]++xs, a > fst average]

optimize :: [Int] -> Int -> Int -> Int
optimize small spare result
  | (null small || spare < 0) = result
  | (abs (sum small)) /= spare = -1
  | head small < 0 = optimize ([(head small)+2]++(tail small)) (spare-2) (result+1)
  | otherwise = optimize (tail small) spare result
  
possible :: [Int] -> Bool
possible [] = True
possible [x]
  | rem x 2 == 0 = True
  | otherwise = False
possible (x:xs) 
  | rem x 2 == 0 = possible $ tail [x]++xs
  | otherwise = False

avg :: [Int] -> (Int, Bool)
avg [] = (0, True)
avg [x] = (x, True)
avg (x:xs) =  
  (div (sum xxs) (length xxs), rem (sum xxs) (length xxs) == 0) 
	where xxs = [x] ++ xs
	      sum' xxs = sum $ take (length xxs) xxs