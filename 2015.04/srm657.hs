import Data.List

isCorrect boards = 
  (== 16) . length $
  takeWhile (==True) $
  map check $ 
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
                     ".......R"] -- True

  print $ isCorrect ["........",
                     "....R...",
                     "........",
                     ".R......",
                     "........",
                     "........",
                     "..R.....",
                     "........"] -- False

  print $ isCorrect ["......R.",
                     "....R...",
                     "...R....",
                     ".R......",
                     "R.......",
                     ".....R..",
                     "..R.....",
                     ".......R"] -- True

  print $ isCorrect ["......R.",
                     "....R...",
                     "...R....",
                     ".R......",
                     "R.......",
                     ".......R",
                     "..R.....",
                     ".......R"] -- False

  print $ isCorrect ["........",
                     "........",
                     "........",
                     "........",
                     "........",
                     "........",
                     "........",
                     "........"] -- False