import Data.Char (digitToInt)
-- from module Data.Char import function digiToInt
-- to convert a char to an int

-- create a function to get the most common bit from  a bit number
mostCommonBit :: String -> Char
mostCommonBit bit_number
  | length (filter (== '0') bit_number) > length bit_number `div` 2 = '0'
  | otherwise = '1'
  -- if 0 appears more than the half of the length of the bit number
  -- return 0
  -- otherwise return 1

-- create a function to convert a string that represent a bit number to an int
bintodec :: String -> Int
bintodec bin=sum [2 ^ snd x * digitToInt (fst x) | x <- zip (reverse bin) [0..]]
-- reverse the bit number and zip it with an infinite array that starts at 0
-- make an array with 0 if the bit is zero
-- and with 2^(current index of number) if the bit
-- return the sum of the array

-- create the main function
main :: IO()
main=do
  let array=["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
  -- set the input array

  let gamma=[mostCommonBit z | z <- [[ y !! x | y <- array] | x <- [0..length (head array)-1]]]
  -- loop to the range of 0-(length of the first element of the array)
  -- and loop the array to construct a new array with bits for the all positions
  -- get most common bit for the each item of a new array

  let epsilon=[if x=='0' then '1' else '0' | x <-gamma]
  -- construct epsilon by fliping the bits of gamma

  print (bintodec gamma * bintodec epsilon)
  -- multiply int value of gamma with int value of epsilon
  -- and print the result