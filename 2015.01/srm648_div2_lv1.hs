import Data.List

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- unfoldr f b  =
--   case f b of
--     Just (a,new_b) -> a : unfoldr f new_b
--     Nothing        -> []

unfoldr' :: Num i => (b -> i -> Maybe (a, b)) -> b -> [a]
unfoldr' f b = unfoldr_iter f b 1
  where unfoldr_iter f b i = 
          case f b i of
            Just (a,new_b) -> a : unfoldr_iter f new_b (i+1)
            Nothing        -> []

num_bougth k t = length $ unfoldr' 
  (\acc i ->  case () of
    _ | acc <= 0 -> Nothing 
      | otherwise -> Just (acc, acc - k*2^(i-1)) ) t

main = do
  print $ unfoldr (\acc-> if acc==0 then Nothing else Just (acc, acc-1)) 10 
  print $ num_bougth 100 300
  print $ num_bougth 150 1050
  print $ num_bougth 160 163680