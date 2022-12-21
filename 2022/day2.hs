{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

compare_options :: Int -> Int -> Int
compare_options x y
  | x == y = 3
  | [2,3,1] !! x-1 == y = 0
  | otherwise = 6

main :: IO()
main = do
  string <- readFile "D:\\haskell\\aoc\\2022\\data\\day2.txt"
  let string_lines=lines string
  
  let elf_choices=[if head x == 'A' then 1 else if head x == 'B' then 2 else 3 | x <- string_lines]
  let my_choices=[if last x == 'X' then 1 else if last x == 'Y' then 2 else 3 | x <- string_lines]
  print $
    sum $
      zipWith
        (+)
        my_choices
        [compare_options x y | (x,y) <- zip my_choices elf_choices]