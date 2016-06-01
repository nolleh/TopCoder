import Data.List
import Data.Maybe

filipTheFrog list leap = 
  (length $
  takeWhile ((<= leap). snd) $
  reverse $
  fst makeSplited)
  +
  (length $ 
  takeWhile ((\d -> (leap >= d)). snd) $ 
  snd makeSplited)
  
  where tuples = zip sorted (0 :(diff sorted $ tail sorted)) -- [(me, diff)] O(N)
        sorted = sort list -- O(nLogn)
        initial = head list
        makeSplited = splitAt (fromJust $ (elemIndex initial $ map fst tuples)) tuples -- O(N^2)

diff [] _ = []
diff _ [] = []
diff (x:xs) (y:ys) = (y - x) : (diff xs ys)


main = do
  print $ filipTheFrog [4, 7, 1, 3, 5] 1  -- 3
  print $ filipTheFrog [100, 101, 103, 105, 107] 2 -- 5
  print $ filipTheFrog [17, 10, 22, 14, 6, 1, 2, 3] 4 -- 7
  print $ filipTheFrog [0] 1000 -- 1
