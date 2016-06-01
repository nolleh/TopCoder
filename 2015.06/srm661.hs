import Data.List
import Data.List.Split

simulate = 
  transpose . 
  map mySort . 
  transpose

-- string [oox.o] split whenX, sort, append
mySort = 
  concat .
  foldr (\s acc -> if (null acc) then s : acc 
                  else s:("x" : acc)) [] .
  map sort .
  splitOn "x"

main = do
  print $ mySort "oxo.o..x." 
  -- ["o","x","...oo","x","."]
  print $ simulate ["ooooo",
                    "..x..",
                    "....x",
                    ".....",
                    "....o"] 
-- ["..o..","..x.o","....x",".....","oo.oo"]

  print $ simulate ["..o..", 
                    "..x.o", 
                    "....x", 
                    ".....",
                    "oo.oo"]
-- ["..o..", "..x.o", "....x", ".....", "oo.oo"]

  print $ simulate ["ooooxooo.ooxo.oxoxoooox.....x.oo"] -- "ooooxooo.ooxo.oxoxoooox.....x.oo"
  print $ simulate ["o",
                    ".",
                    "o",
                    ".",
                    "o",
                    ".",
                    "."]
-- [".", ".", ".", ".", "o", "o", "o" ]

  print $ simulate ["oxxxxooo",
                    "xooooxxx",
                    "..xx.ooo",
                    "oooox.o.",
                    "..x....."]
-- ["oxxxxooo",  "x.oo.xxx", "..xxo...", ".oo.x.o.", "ooxo.ooo"]

  print $ simulate ["..o..o..o..o..o..o..o..o..o..o..o",
                    "o..o..o..o..o..o..o..o..o..o..o..",
                    ".o..o..o..o..o..o..o..o..o..o..o.",
                    "...xxx...xxx...xxxxxxxxx...xxx...",
                    "...xxx...xxx...xxxxxxxxx...xxx...",
                    "...xxx...xxx......xxx......xxx...",
                    "...xxxxxxxxx......xxx......xxx...",
                    "...xxxxxxxxx......xxx......xxx...",
                    "...xxxxxxxxx......xxx......xxx...",
                    "...xxx...xxx......xxx............",
                    "...xxx...xxx...xxxxxxxxx...xxx...",
                    "...xxx...xxx...xxxxxxxxx...xxx...",
                    "..o..o..o..o..o..o..o..o..o..o..o",
                    "o..o..o..o..o..o..o..o..o..o..o..",
                    ".o..o..o..o..o..o..o..o..o..o..o."]

                    -- ".................................",
                    -- ".................................",
                    -- "...ooo...ooo...ooooooooo...ooo...",
                    -- "...xxx...xxx...xxxxxxxxx...xxx...",
                    -- "...xxx...xxx...xxxxxxxxx...xxx...",
                    -- "...xxxoooxxx......xxx......xxx...",
                    -- "...xxxxxxxxx......xxx......xxx...",
                    -- "...xxxxxxxxx......xxx......xxx...",
                    -- "...xxxxxxxxx......xxx......xxx...",
                    -- "...xxx...xxx......xxx............",
                    -- "...xxx...xxx...xxxxxxxxx...xxx...",
                    -- "...xxx...xxx...xxxxxxxxx...xxx...",
                    -- ".................................",
                    -- "ooo.........ooo.........ooo...ooo",
                    -- "ooooooooooooooooooooooooooooooooo"


