{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.List (nub)
import Data.Array (Ix(range))

second_number :: String -> String
second_number str=
  if head str==',' then tail str
  else second_number (tail str)

filter_index :: [(Int,Int)] -> (Int -> Bool) -> [(Int,Int)]
filter_index list fun=[x | (x,i) <- zip list [0..], fun i]

even_indeces :: [(Int,Int)] -> [(Int,Int)]
even_indeces list=filter_index list even

odd_indeces :: [(Int,Int)] -> [(Int,Int)]
odd_indeces list=filter_index list odd

is_linear :: (Int,Int) -> (Int,Int) -> Bool
is_linear a b
  | fst a == fst b || snd a == snd b = True
  | otherwise = False

sort :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
sort tuple
  | (a == c  && b > d) || (b == d  && a > c) = (snd tuple,fst tuple)
  | otherwise = tuple
  where
    a=fst $ fst tuple
    b=snd $ fst tuple
    c=fst $ snd tuple
    d=snd $ snd tuple

main :: IO()
main=do
  let array=["0,9 -> 5,9","8,0 -> 0,8","9,4 -> 3,4","2,2 -> 2,1","7,0 -> 7,4","6,4 -> 2,0","0,9 -> 2,9","3,4 -> 1,4","0,0 -> 8,8","5,5 -> 8,2"]

  let coordinates=[
          (
            read $ reverse $ second_number $ reverse y,
            read $ second_number y
          )
            | x <- array,
              y <- filter (/= "->") $ words x
        ]

  let linearcoordinates=[
          sort x
            | x <- zip (even_indeces coordinates) (odd_indeces coordinates),
              uncurry is_linear x
        ]
  -- zip even_indeces array and odd_indeces arsay to get the tuples with start and stop coordinates
  -- uncurry to separate in two different arguments
  -- to filter points that aren't is_linear
  -- and sort the remaining tuples

  print $
    (-)
      (length $ [y | x <- linearcoordinates, y <- range x])
      (length $ nub [y | x <- linearcoordinates, y <- range x])
  -- use range function to generate all points in these ranges
  -- apply length to get the number of points
  -- similar for the set of this array
  -- substract lenght of set from lenght of array
  -- and print the result