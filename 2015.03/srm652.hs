import Data.Char

ords :: [Char] -> [Int]
ords xs = [ord a - 96 | a <- xs]

find_value str = sum $ evals (ords str) (ords str) 

evals :: [Int] -> [Int] -> [Int]
evals origin = map (eval origin)

eval :: [Int] -> Int -> Int
eval rest el = el * length (filter (<= el) rest)

main = do
  print $ find_value "babca" -- 35
  print $ find_value "zz"    -- 104
  print $ find_value "y"     -- 25
  print $ find_value "aaabbc" -- 47
  print $ find_value "topcoder" -- 558
  print $ find_value "thequickbrownfoxjumpsoverthelazydog" --11187
  print $ find_value "zyxwvutsrqponmlkjihgfedcba"          --6201