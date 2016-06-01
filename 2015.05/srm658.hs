import Data.List
equal str str2 = 
  (==) (str_seq str) (str_seq str2)
  where 
    gen a = [x | x <- map (take' a) [1..(length a)]]
    str_seq a = find (\c -> (== "") $ (myDrop a c)) (gen a)
    
take' str len = take len str

-- 1 에서 2 뗌
myDrop [] _ = []
myDrop str1 str2 
  | (length str2 <= length str1) && (str2 == takes2') = (myDrop drops2' str2)
  | otherwise = str1
  where takes2' = take (length str2) str1
        drops2' = drop (length str2) str1

main = do
  print $ equal "ab" "abab" -- true
  print $ equal "abc" "bca" -- false
  print $ equal "abab" "aba" -- false
  print $ equal "aaaaa" "aaaaaa" -- true
  print $ equal "ababab" "abab" -- true
  print $ equal "a" "z" -- false
  print $ equal "abaaba" "aba" -- true



