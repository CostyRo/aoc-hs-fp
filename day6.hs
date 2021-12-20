-- create a function to get the number for next day
nextDay :: Int -> Int 
nextDay 0=6
nextDay n=n-1
-- if current day is 0 return 6
-- otherwise return number of days - 1

-- create a recursive function to get the array with fishes after n days
simulate :: Int -> [Int] -> [Int]
simulate 0 list=list
simulate n list=simulate (n-1) ([nextDay x | x <- list] ++ [8 | _ <- [0..length(filter (== 0) list)-1]])
-- if base case is trigger return the array
-- return a recursion with number of days - 1 and
-- a new array formed by concatenating mapped array with nextDay function
-- and a array with 8s as many 0s are in the mapped array

-- create the main function
main :: IO()
main=do
  let array=[3,4,3,1,2]
  -- set the input array

  let days=80
  -- set the input days

  print (length (simulate days array))
  -- get the length for the simulation
  -- and print the result