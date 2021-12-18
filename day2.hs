import Data.List (isPrefixOf)

split :: [Char] -> [Char]
split str =
  if head str==' ' then tail str
  else split (tail str)

main :: IO ()
main=do
  let arr=["forward 5","down 5","forward 8","up 3","down 8","forward 2"]
  print (sum [(read :: String -> Int) (split x) | x <- arr, isPrefixOf "forward" x] * (sum [(read :: String -> Int) (split x) | x <- arr, isPrefixOf "down" x] - sum [(read :: String -> Int) (split x) | x <- arr, isPrefixOf "up" x]))