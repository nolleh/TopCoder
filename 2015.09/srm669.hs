import Data.Function (on)
import Data.List

maxHappiness point singer =
  sum $ 
  map (fst.head.take 1.sortBy sortFst) $ 
  groupBy ((==) `on` snd) $
  sortBy sortSnd $ 
  zip point singer

sortFst (a,b) (a2,b2)
  | a < a2 = GT
  | a > a2 = LT
  | otherwise = compare a a2

sortSnd (a,b) (a2,b2)
  | b < b2 = LT
  | b > b2 = GT
  | otherwise = compare b b2

main = do
  print $ maxHappiness [10,5,6,7,1,2] ["ciel","bessie","john","bessie","bessie","john"]
  print $ maxHappiness [3,3,4,3,3] ["a","a","a","a","a"]
  print $ maxHappiness [1,2,3,4,5,6,7,8,9,10,100] ["a","b","c","d","e","e","d","c","b","a","abcde"]
  print $ maxHappiness [100] ["oyusop"]
  print $ maxHappiness [100,100,100,100,100,100,100,100,100,100,100,100,100] ["haruka","chihaya","yayoi","iori","yukiho","makoto","ami","mami","azusa","miki","hibiki","takane","ritsuko"]

  -- 23, 4, 140, 100, 1300 