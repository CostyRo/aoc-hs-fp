{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.Char (digitToInt)

most_common_bit :: String -> Char
most_common_bit bit_number
  | (length . filter (== '0')) bit_number > length bit_number `div` 2 = '0'
  | otherwise = '1'

bintodec :: String -> Int
bintodec bin_number=
  sum
    [
      bin_formula x
        | x <- (zip . reverse) bin_number [0..]
    ]
  where bin_formula x=
          2 ^ snd x * digitToInt (fst x)

main :: IO()
main=do
  let array=["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]

  let bits_positions=[
          [y !! x | y <- array]
            | x <- [0..(-) ((length . head) array) 1]
        ]

  let gamma=[
          most_common_bit z
            | z <- bits_positions
        ]

  let epsilon=[
          if x=='0' then '1' else '0'
            | x <-gamma
        ]

  print $
    (*)
      (bintodec gamma)
      (bintodec epsilon)