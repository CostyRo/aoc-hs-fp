{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.List (intersect,elemIndex)

to_int :: Maybe Int -> Int
to_int (Just a)=a

main :: IO()
main = do
  string <- readFile "D:\\haskell\\aoc\\2022\\data\\day3.txt"
  let string_lines=lines string

  let compartiments=[splitAt (length x `div` 2) x | x <- string_lines]
  let intersections=[head $ x `intersect` y | (x,y) <- compartiments]

  print $
    sum
      [
        ((+) . to_int)
          (x  `elemIndex` (++) ['a'..'z'] ['A'..'Z'])
          1
            | x <- intersections
      ]