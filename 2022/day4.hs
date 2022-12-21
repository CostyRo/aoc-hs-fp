{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

split :: Char -> String -> [String]
split delim [] = [""]
split delim (x:xs)
  | x == delim = "" : rest
  | otherwise = (x : head rest) : tail rest
    where rest = split delim xs

filter_index :: [[Int]] -> (Int -> Bool) -> [[Int]]
filter_index list fun=[x | (x,i) <- zip list [0..], fun i]

even_indeces :: [[Int]] -> [[Int]]
even_indeces list=filter_index list even

odd_indeces :: [[Int]] -> [[Int]]
odd_indeces list=filter_index list odd

overlap :: [Int] -> [Int] -> Bool
overlap x y
  | head x <= head y && last x >= last y = True
  | head x >= head y && last x <= last y = True
  | otherwise = False

main :: IO()
main = do
  string <- readFile "D:\\haskell\\aoc\\2022\\data\\day4.txt"
  let string_lines=lines string

  let matrix=[
          map read $ split '-' y
            | x <- string_lines,
              y <- split ',' x
        ]

  let (first_lines,second_lines)=
        (even_indeces matrix,odd_indeces matrix)

  print $
    length $
      filter
      (== True) $
      zipWith
        overlap
        first_lines
        second_lines