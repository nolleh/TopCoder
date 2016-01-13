
-- perform_the_experiment :: [Int] -> [Int]
-- perform_the_experiment list
--   | 3 < length list = (perform_one $ take 3 list):(perform_the_experiment $ tail list)
--   | otherwise = list

replicate_edge:: [Int] -> [Int]
replicate_edge list = (head list) : list ++ [(last list)]

extract :: [Int] -> [[Int]]
extract (_:[]) = []
extract (x:y:[]) = []
extract list@(x:y:xs) = 
  (take 3 list) : (extract $ y:xs)

perform_one :: [Int] -> Int
perform_one = do left <- head
                 me <- head.tail
                 right <- head.tail.tail
                 if (left > me && right > me) then return (me + 1)
                 else if (left < me && right < me) then return (me - 1)
                 else return me
  
main = do
  -- print $ perform_one [4,6,4]
  -- print $ extract $ replicate_edge [5, 2,3,4, 1]
  -- print $ replicate_edge [2,3,4]
  print $ map (\x -> perform_one x) (extract $ replicate_edge [5,3,4,6,1])
  -- print $ perform_the_experiment [5, 3, 4, 6, 1]