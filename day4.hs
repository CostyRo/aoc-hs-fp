import Data.List ((\\),elemIndex,transpose)

eliminate :: Maybe Int -> Int
eliminate (Just a)=a

minIndex :: [Int] -> Int
minIndex x=head (filter ((== minimum x) . (x !!)) [0..])

minimumArrayIndexForMatrices :: [Int] -> [[[Int]]] -> [Int]
minimumArrayIndexForMatrices array matrices=[minimum [maximum (map (eliminate . (`elemIndex` array)) y) | y <- x] | x <- matrices]

main :: IO()
main=do
  let array=[7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
  let matrices=[
                [
                  [22,13,17,11,0],
                  [8,2,23,4,24],
                  [21,9,14,16,7],
                  [6,10,3,18,5],
                  [1,12,20,15,19]
                ],
                [
                  [3,15,0,2,22],
                  [9,18,13,17,5],
                  [19,8,7,25,23],
                  [20,11,10,24,4],
                  [14,21,16,12,6]
                ],
                [
                  [14,21,17,24,4],
                  [10,16,15,9,19],
                  [18,8,23,26,20],
                  [22,11,13,6,5],
                  [2,0,12,3,7]
                ]
               ]
  let winningMatrixIndex=minIndex (zipWith min (minimumArrayIndexForMatrices array matrices) (minimumArrayIndexForMatrices array [transpose x | x <- matrices]))
  let finalNumber=array !! min (minimum (minimumArrayIndexForMatrices array matrices)) (minimum (minimumArrayIndexForMatrices array [transpose x | x <- matrices]))
  let remainingSum=sum ([y | x <- matrices !! winningMatrixIndex, y <- x] \\ take (eliminate (elemIndex finalNumber array)+1) array)
  print (remainingSum * finalNumber)