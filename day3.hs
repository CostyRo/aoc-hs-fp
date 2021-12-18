import Data.Char (digitToInt)

mostCommonBit :: String -> Char
mostCommonBit bit_number
  | length (filter (== '0') bit_number) > length bit_number `div` 2 = '0'
  | otherwise = '1'

bintodec :: String -> Int
bintodec bin=sum [2 ^ snd x * digitToInt (fst x) | x <- zip (reverse bin) [0..]]

main :: IO()
main=do
  let array=["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
  let gamma=[mostCommonBit z | z <- [[ y !! x | y <- array] | x <- [0..4]]]
  let epsilon=[if x=='0' then '1' else '0' | x <-gamma]
  print (bintodec gamma * bintodec epsilon)