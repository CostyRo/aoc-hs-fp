{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

next_day :: Int -> Int
next_day 0=6
next_day n=n-1

simulate :: Int -> [Int] -> [Int]
simulate 0 list=list
simulate n list=
  simulate
    (n-1) $
    (++)
      [next_day x | x <- list]
      [8 | _ <- [0..(-) (length $ filter (== 0) list) 1]]

main :: IO()
main=do
  let array=[3,4,3,1,2]

  let days=80

  print $
    length $
      simulate
        days
        array