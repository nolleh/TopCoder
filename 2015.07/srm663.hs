import Data.List

produce strs = map frequency strs

isCheckerBoard strs
  | (distinctCheck == True) = 
      (all (==True) . map allTheSame $ rowDiagonal strs) && 
      (all (==True) . map allTheSame $ colDiagonal strs)
  | otherwise = False
  where rowDiagonal ss = --["aaa","bb","a"]
          map diagonal $ 
          map (\(i, s) -> iterate tail s !! i) $
          zip [0..] $ replicate (length ss) ss
        allTheSame xs = and $ map (== head xs) (tail xs)
        colDiagonal = rowDiagonal . transpose
        distinctCheck
          | (length $ frequency strs) == 2 = True
          | otherwise = False

frequency :: Ord a => [a] -> [(Int,a)] 
frequency list = map (\l -> (length l, head l)) (group (sort list))

diagonal :: [[a]] -> [a]
diagonal []     = []
diagonal (x:xs) = head x : diagonal (map tail xs)

main = do
  print $ isCheckerBoard ["aba", "bab", "cba"]
  print $ isCheckerBoard ["aba", "bab", "aba"]