import Data.Char 

toHex :: Int -> String
toHex = map toUpper . reverse . recurse
  where recurse n
          | n < 16 = [ intToDigit n ]
          | otherwise = let (q,r) = n `divMod` 16
                        in (intToDigit r) : recurse q

hexspeak = refine . toHex

refine hex
  | any (\c -> (1 < digitToInt c) && (digitToInt c < 10)) hex = "Error!"
  | otherwise = hex10 hex

hex10 = map convert
  where convert c
          | c == '1' = 'I'
          | c == '0' = 'O'
          | otherwise = c

main = do
  print $ hexspeak 257
  print $ hexspeak 258
  print $ hexspeak 3405691582
  print $ hexspeak 2882400001
  print $ hexspeak 999994830345994239
  print $ hexspeak 1000000000000000000
