import Data.List (nub)
import Data.Array (Ix(range))
-- from module Data.List import function nub
-- to get the set of an array
-- from module Data.List import function range from Ix
-- to calculate all the numbers between 2 points

-- create a function to get the number of a command
split :: String -> String
split str=
  if head str==',' then tail str
  -- if the string starts with a comma return tail of it
  else split (tail str)
  -- otherwise call the function recursively with the tail of the string
  -- until the string will starts with a comma

-- create a function to get only the elements at an even index
evens :: [(Int,Int)] -> [(Int,Int)]
evens list=[x | (x,i) <- zip list [0..], even i]
-- enumerate list add filter elements which doesn't have an even index

-- create a function to get only the elements at an odd index
odds :: [(Int,Int)] -> [(Int,Int)]
odds list=[x | (x,i) <- zip list [0..], odd i]
-- same, but for odds

-- create a function to filter points which aren't linear
linear :: (Int,Int) -> (Int,Int) -> Bool
linear x y
  | fst x == fst y || snd x == snd y= True
  | otherwise = False
  -- if the points have common x or commony return True
  -- oherwise return False

-- create a function that sorts tuples
sort :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
sort tuple
  | (a == c  && b > d) || (b == d  && a > c) = (snd tuple,fst tuple)
  | otherwise = tuple
  where a=fst (fst tuple)
        b=snd (fst tuple)
        c=fst (snd tuple)
        d=snd (snd tuple)
  -- if start point have bigger coordinates than stop point
  -- reverse the tuple
  -- otherwise return the normal tuple
  -- i used where to simplify writing elements of tuples

-- create the main function
main :: IO()
main=do
  let array=["0,9 -> 5,9","8,0 -> 0,8","9,4 -> 3,4","2,2 -> 2,1","7,0 -> 7,4","6,4 -> 2,0","0,9 -> 2,9","3,4 -> 1,4","0,0 -> 8,8","5,5 -> 8,2"]
  -- set the input array

  let coordinatesList=[((read :: String -> Int) (reverse (split (reverse y))), (read :: String -> Int) (split y)) | x <- array, y <- filter (/= "->") (words x)]
  -- loop the array an aply words on each elements
  -- filter result to thrown down the arrow
  -- reverse the element, split it and reverse it again to get the x coordinate
  -- and only split the element to get the y coordinate
  -- convert them to int and put them in a tuple

  let linearCoordinatesList=[sort x | x <- zip (evens coordinatesList) (odds coordinatesList), uncurry linear x]
  -- zip evens list and odds list to get the tuples with start and stop coordinates

  print (length ([y | x <- linearCoordinatesList, y <- range x]) - length (nub [y | x <- linearCoordinatesList, y <- range x]))
  -- and print the result