import Data.Bits

luckyXor :: Int -> Int
luckyXor a = 
  get $
  filter (\(b,x)-> checkLucky x) $ 
  map (\b -> (b, xor a b)) [a+1..100]

get xs
  | null xs = -1
  | otherwise =  fst . head $ take 1 xs

checkLucky n = 
  all (\x-> (x == 4) || (x == 7)) $
  reverse $ digits n

digits n = reveresedDigit
  where reveresedDigit
          | n < 10 = [n]
          | otherwise = (mod n 10) : (digits (div n 10))

main = do
  print $ luckyXor 4
  print $ luckyXor 19
  print $ luckyXor 88
  print $ luckyXor 36