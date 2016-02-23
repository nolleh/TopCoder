
check s t 
  | length t + 1 /= length s = "Impossible"
  | False == check_res = "Impossible"
  | otherwise = "Possible"
  where check_res = any (==t) [delete i s | i <- [0..length t]]


delete n xs = let (ys,zs) = splitAt n xs in ys ++ (tail zs)

main = do
  print $ check "sunuke" "snuke"
  print $ check "snuke" "snuke"
  print $ check "snuke" "skue"
  print $ check "snukent" "snuke"
  print $ check "aaaaa" "aaaa"
  print $ check "aaaaa" "aaa"
  print $ check "topcoder" "tpcoder"
  print $ check "singleroundmatch" "singeroundmatc"
