
-- 나를 빼고 tail 이 온다
select_adjust :: Int -> [Int] -> Int
select_adjust this = 
  minimum.map (\x -> (abs(this - x) -1))

list_adjust (_:[]) = []
list_adjust (x:xs) = (select_adjust x xs):(list_adjust xs)

find :: Int -> [Int] -> Int
find conse_num list
  | conse_num == 1 = 0
  | otherwise = minimum $ list_adjust list

main = do
  print $ select_adjust 1 [50,-3,4]
  print $ find 2 [4, 47, 7]
  print $ find 1 [1, 100]
  print $ find 2 [-96, -53, 82, -24, 6, -75]
  print $ find 2 [64, -31, -56]

-- 2
-- 2
-- 0
-- 20
-- 24
-- [Finished in 0.4s]