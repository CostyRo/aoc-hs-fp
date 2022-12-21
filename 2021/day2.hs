{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.List (isPrefixOf)

command_number :: String -> String
command_number str
  | head str==' ' = tail str
  | otherwise = command_number $ tail str

filtered_array :: [String] -> String -> [Int]
filtered_array array command=[
    read $ command_number x
      | x <- array,
        command `isPrefixOf` x
  ]

main :: IO()
main=do
  let array=["forward 5","down 5","forward 8","up 3","down 8","forward 2"]

  print $
    (*)
      (sum $ filtered_array array "forward") $
      (-)
        (sum $ filtered_array array "down")
        (sum $ filtered_array array "up")