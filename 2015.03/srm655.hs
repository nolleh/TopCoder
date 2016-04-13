import Data.List

ableToDraw list = 
  makeOutPut $ 
  all (==True) $
  map (wrapSolve ' ' 0) list

makeOutPut bool
  | bool = "possible"
  | otherwise = "impossible"

wrapSolve :: Char -> Int -> String -> Bool
wrapSolve ckWord count list = myGroup list ckWord count

myGroup :: String -> Char -> Int -> Bool
myGroup [] _ _ = True
myGroup list@(x:xs) ckWord count
  | (' ' /= ckWord && x == '?') = 
      (myGroup (dropWhile (=='?') xs) ckWord (length.head $ group list))
  | (x /= '?' && count == 0) = myGroup xs x 0
  | (ckWord == x && mod count 2 /= 0) = myGroup xs ' ' count
  | (ckWord /= x && mod count 2 == 0) = myGroup xs ' ' count
  | otherwise = False

main = do
  print $ ableToDraw ["W?W",
                      "??B",
                      "???"]
  print $ ableToDraw ["W??W"]
  print $ ableToDraw ["??"]
  print $ ableToDraw ["W???",
                      "??B?",
                      "W???",
                      "???W"]
  print $ ableToDraw ["W???",
                      "??B?",
                      "W???",
                      "?B?W"]
  print $ ableToDraw ["B"]