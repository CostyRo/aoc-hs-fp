{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

split_by_value :: String -> [String] -> [[String]]
split_by_value item list = f list []
  where
    f [] buffer = [buffer]
    f (x:xs) buffer
      | x==item = buffer : f xs []
      | otherwise = f xs $ (++) buffer [x]
-- use an auxiliar function(f) that constructs a buffer
-- and concatenates lists when item is found as head

main :: IO()
main = do
  string <- readFile "D:\\haskell\\aoc\\2022\\data\\day1.txt"
  let string_lines=lines string

  let max_calories=
        maximum
          [
            sum [(read :: String -> Int) y | y <- x]
              | x <- split_by_value "" string_lines
          ]

  print max_calories