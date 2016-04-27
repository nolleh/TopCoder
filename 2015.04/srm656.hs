import Data.Char
mySolve :: Int -> String -> String
mySolve corCnt str@(x:xs)
  | corCnt == 0 = str
  | otherwise =
      (ck (length str) str) $
      -- (2, 'l'), (2, 'l')
      filter (\(l, c) -> remain_count == l) $
      -- [(1, 'h'), (1, 'e'), (2, 'l'), (2, 'l') (1, 'o')]
      map (\c ->  (length $ (filter (==c) str), c)) str
        where remain_count = length str - corCnt

ck :: Int -> String -> [(Int, Char)] -> String
ck cnt str [] = replicate cnt $ pick str
ck cnt _ (x:_) = replicate cnt $ snd x

pick :: String -> Char
pick str = head $ filter (\c-> not $ elem c str) "abcdefghijklmnopqrstuvwxyz"

main = do
  print $ mySolve 3 "hello"
  print $ mySolve 3 "abc"
  print $ mySolve 0 "wwwwwwwwwwwwwwwwww"
  print $ mySolve 3 "ababba"
  print $ mySolve 10 "zoztxtoxytyt"
  print $ mySolve 13 "jlmnmiunaxzywed"
  -- print $ ord 'a'