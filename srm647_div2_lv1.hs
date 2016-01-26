--peaceful line
--1. brute force
replicateM n = sequence.replicate n

distinct_elems (x:[]) = True
distinct_elems (x:xs)
  | elem x xs = False
  | otherwise = distinct_elems xs

make_line list = any (\xs -> distinct_elems xs) $ replicateM (length list) list

main = do
  -- print $ find (/= 1) [1]
  -- [[1,1,2,2,3,3,4,4],[],[],[]]

  -- print $ replicateM 4 [1,1,2,2,3,3,4,4] -- 8 ^ 4 = 4096 무진장 느리다.
  -- print $ distinct_elems [1,2,3,4,6]
  -- print $ any (\xs -> distinct_elems xs) [[1,1,3], [1,1,3]]
  print $ replicateM 4 [1,2,3,4] -- poss = 4^4
  print $ make_line [1,1,1,2] -- imposs
  -- print $ make_line [1,1,2,2,3,3,4,4] -- poss
  -- print $ make_line [3,3,3,3,13,13,13,13] -- poss
  -- print $ make_line [3,7,7,7,3,7,7,7,3] -- imposs
  -- print $ make_line [25,12,3,25,25,12,12,12,12,3,25] --poss
  -- print $ make_line [3,3,3,3,13,13,13,13,3] -- poss
