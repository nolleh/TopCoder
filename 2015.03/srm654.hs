import Data.List

square_scores_div2 :: [Char] -> Int
square_scores_div2 str = 
  sum $ -- n => 3n => O(n)
  concatMap -- [[3C1, 3C2, 3C3] ++ [2C1]...]
    (\subs -> let size = (length subs) in -- (3 + 2 + 1)*2 => 2n 
      map (c size) [1..size]) $ -- [[3C1, 3C2, 3C3],[2C1]...], 
  group str -- ["zzz", "xx", "zz"], n

c :: Int -> Int -> Int
c n r = n - r + 1

main = do
  print $ square_scores_div2 "aaaba" -- 8
  print $ square_scores_div2 "zzzxxzz" -- 12
  print $ square_scores_div2 "abcdefghijklmnopqrstuvwxyz" -- 26
  print $ square_scores_div2 "p" -- 1
  print $ square_scores_div2 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" -- 5050