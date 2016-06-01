get_number :: Int -> Int -> Int
get_number a b = min (get a) (get b) + (refine a b)

get :: Int -> Int
get a
  | (rem a 2) /= 0 = 0
  | otherwise = 1 + get (div a 2)

refine :: Int -> Int -> Int
refine a b
  | (rem a b) == 0 || (rem b a) == 0 = 1
  | otherwise = 0

main = do
-- 8 / 4, 2, 1
-- 4 / 2, 1
  print $ get_number 8 4 -- 3
  print $ get_number 4 7 -- 0
  print $ get_number 12 12 -- 3
  print $ get_number 24 96 -- 4
  print $ get_number 1000000000 999999999 -- 0
