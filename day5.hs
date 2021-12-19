import Data.List (nub)
import Data.Array (Ix(range))

split :: String -> String
split str=
  if head str==',' then tail str
  else split (tail str)

evens :: [a] -> [a]
evens list=[x | (x,i) <- zip list [0..], even i]

odds :: [a] -> [a]
odds list=[x | (x,i) <- zip list [0..], odd i]

linear :: (Int,Int) -> (Int,Int) -> Bool
linear x y
  | fst x == fst y || snd x == snd y= True
  | otherwise = False

sort :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
sort tuple
  | (a == c  && b > d) || (b == d  && a > c) = (snd tuple,fst tuple)
  | otherwise = tuple
  where a=fst (fst tuple)
        b=snd (fst tuple)
        c=fst (snd tuple)
        d=snd (snd tuple)

main :: IO()
main=do
  let array=["0,9 -> 5,9","8,0 -> 0,8","9,4 -> 3,4","2,2 -> 2,1","7,0 -> 7,4","6,4 -> 2,0","0,9 -> 2,9","3,4 -> 1,4","0,0 -> 8,8","5,5 -> 8,2"]
  let coordinatesList=[((read :: String -> Int) (reverse (split (reverse y))), (read :: String -> Int) (split y)) | x <- array, y <- filter (/= "->") (words x)]
  let linearCoordinatesList=[sort x | x <- zip (evens coordinatesList) (odds coordinatesList), uncurry linear x]
  print (length ([y | x <- linearCoordinatesList, y <- range x]) - length (nub [y | x <- linearCoordinatesList, y <- range x]))