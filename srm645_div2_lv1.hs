replicate_edge:: [Int] -> [Int]
replicate_edge list = (head list) : list ++ [(last list)]

triple_tie :: [Int] -> [[Int]]
triple_tie (_:[]) = []
triple_tie (x:y:[]) = []
triple_tie list@(x:y:xs) = 
  (take 3 list) : (triple_tie $ y:xs)

perform_elem :: [Int] -> Int
perform_elem = do left <- (!!0)
                  me <- (!!1)
                  right <- (!!2)
                  if (left > me && right > me) then return (me + 1)
                  else if (left < me && right < me) then return (me - 1)
                  else return me

perform_once = map (\x -> perform_elem x).triple_tie.replicate_edge

perform_the_experiment xs
  | (== perform_once xs) xs = xs
  | otherwise = perform_the_experiment.perform_once $ xs

main = do
  print $ perform_the_experiment [5, 3, 4, 6, 1]
  --Returns: {5, 4, 4, 4, 1 }
  print $ perform_the_experiment [1, 5, 4, 9]
  --Returns: {1, 4, 5, 9 }
  print $ perform_the_experiment [78, 34, 3, 54, 44, 99]
  --Returns: {78, 34, 34, 49, 49, 99 }
  print $ perform_the_experiment [32, 68, 50, 89, 34, 56, 47, 30, 82, 7, 21, 16, 82, 24, 91]
  --Returns: {32, 59, 59, 59, 47, 47, 47, 47, 47, 18, 18, 19, 53, 53, 91 }

