import Data.List
encrypt :: String -> [Int] -> Int -> String
encrypt str locs rep = iterate (enc locs) str !! rep

-- iterate 함수는 인자와 결과 타입이 같아야 한다.
-- iterate :: (a -> a) -> a -> a
-- 그래서 enc 순서 바꿔서 사용함.
enc :: [Int] -> String -> String
enc locs str = 
  map fst $
  sortBy sortSnd $
  zip str locs

sortSnd (a1, b1) (a2, b2)
  | b1 < b2 = LT
  | b1 > b2 = GT
  | b1 == b2 = compare a1 a2

main = do
  print $ encrypt "abc" [1, 2, 0] 1 -- cab
  print $ encrypt "abcde" [4, 3, 2, 1, 0] 1 -- edcba
  print $ encrypt "abcde" [4, 3, 2, 1, 0] 2 -- "abcde"
  print $ encrypt "uogcodlk" [4, 3, 6, 2, 5, 1, 0, 7] 44 -- "goodluck"