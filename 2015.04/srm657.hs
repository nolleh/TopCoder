import Data.List

isCorrect boards = 
  (== 17) . length $
  takeWhile (==True) $
  scanr (\a _ -> check a) True $ 
  boards ++ transpose boards

check :: String -> Bool
check = (== 1) . length . filter (== 'R')

main = do
  print $ isCorrect ["R......",
                     ".R.....",
                     "..R.....",
                     "...R....",
                     "....R...",
                     ".....R..",
                     "......R.",
                     ".......R"]

  print $ isCorrect ["........",
                     "....R...",
                     "........",
                     ".R......",
                     "........",
                     "........",
                     "..R.....",
                     "........"]

  print $ isCorrect ["......R.",
                     "....R...",
                     "...R....",
                     ".R......",
                     "R.......",
                     ".....R..",
                     "..R.....",
                     ".......R"]

  print $ isCorrect ["......R.",
                     "....R...",
                     "...R....",
                     ".R......",
                     "R.......",
                     ".......R",
                     "..R.....",
                     ".......R"]

  print $ isCorrect ["........",
                     "........",
                     "........",
                     "........",
                     "........",
                     "........",
                     "........",
                     "........"]