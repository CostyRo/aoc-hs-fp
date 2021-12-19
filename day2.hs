import Data.List (isPrefixOf)
-- from module Data.List import function isPrefixOf
-- to check if a string starts with another string

-- create a function to get the number of a command
split :: String -> String
split str=
  if head str==' ' then tail str
  -- if the string starts with a whitespace return tail of it
  else split (tail str)
  -- otherwise call the function recursively with the tail of the string
  -- until the string will starts with a whitespace

-- create the main function
main :: IO()
main=do
  let array=["forward 5","down 5","forward 8","up 3","down 8","forward 2"]
  -- set the input array

  print (sum [(read :: String -> Int) (split x) | x <- array, "forward" `isPrefixOf` x] * (sum [(read :: String -> Int) (split x) | x <- array, "down" `isPrefixOf` x] - sum [(read :: String -> Int) (split x) | x <- array, "up" `isPrefixOf` x]))
  -- filter the input array to get only the string which starts with "forward"
  -- thrown down the part that isn't a number and convert number part to an int
  -- now calculate the sum of this array
  -- similarly for "up" and "down"
  -- substract value from up from value from down
  -- and now multiply this with the value for forward
  -- and print the result