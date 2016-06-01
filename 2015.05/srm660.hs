import Data.List

bestMod i instr =
  minRotate $
  snd $ unzip $ -- ([0,1,2,3,4,5],"sgsgaa")
  sortBy sortTuple1 $ 
  (forward sorted1) ++ (backward sorted1)
  where 
    indexedStr = zip [0..]
    forward str = take (length str - i) str
    backward str = map (\(i,c) -> (i, 'a')) $ drop (length str - i) str
    sorted1 = sortBy sortTuple2 $ indexedStr instr

minRotate str = 
  head $
  sort $
  map (\(i, c) -> rotate i c) $
  zip [1..] sl
  where sl = replicate (length str) str

sortTuple1 (a1, b1) (a2, b2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | a1 == a2 = compare b1 b2

sortTuple2 (a1, b1) (a2, b2)
  | b1 < b2 = LT
  | b1 > b2 = GT
  | b1 == b2 = compare a1 a2

rotate n xs = bs ++ as where (as, bs) = splitAt n xs

main = do
  print $ bestMod 1 "aba"
  print $ bestMod 0 "aba" -- aab
  print $ bestMod 2 "bbb" -- aab
  print $ bestMod 1 "sgsgaw" -- aasgsg
  print $ bestMod 1 "abacaba" -- aaaabac
  print $ bestMod 2 "isgbiao" -- aaaisgb