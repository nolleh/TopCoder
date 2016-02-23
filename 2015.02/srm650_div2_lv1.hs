get_number :: Int -> Int -> Int
get_number x y = refine $ logBase 2 (fromIntegral $ gcd x y)

-- realfrac for compare with 0.0
refine :: RealFrac a => a -> Int
refine a
  | a /= 0.0 && (a == (fromIntegral (ceiling a))) = floor (a + 1)
  | otherwise = floor a

main = do
  print $ get_number 8 4 -- 3
  print $ get_number 4 7 -- 0
  print $ get_number 12 12 -- 3
  print $ get_number 24 96 -- 4
  print $ get_number 1000000000 999999999 -- 0
