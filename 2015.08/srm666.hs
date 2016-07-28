devuAndGame list = innerGame list 0 0 50
  where innerGame list i try maxtry
          | try >= maxtry = "lose"
          | list !! i == -1 = "win"
          | otherwise = innerGame list (list !! i) (try + 1) maxtry

main = do 
  print $ devuAndGame [1, -1] -- win
  print $ devuAndGame [1, 0, -1] -- lose
  print $ devuAndGame [0, 1, 2]  -- lose
  print $ devuAndGame [29,33,28,16,-1,11,10,14,6,31,7,35,34,8,15,17,26,12,13,22,1,20,2,21,-1,5,19,9,18,4,25,32,3,30,23,10,27] -- win
  print $ devuAndGame [17,43,20,41,42,15,18,35,-1,31,7,33,23,33,-1,-1,0,33,19,12,42,-1,-1,9,9,-1,39,-1,31,46,-1,20,44,41,-1,-1,12,-1,36,-1,-1,6,47,10,2,4,1,29] --win
  print $ devuAndGame [3, 1, 1, 2, -1, 4] -- lose