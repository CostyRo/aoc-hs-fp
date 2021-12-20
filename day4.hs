import Data.List ((\\),elemIndex,transpose)
-- from module Data.List import operator \\
-- to perform a difference bewtween arrays
-- from module Data.List import function elemIndex
-- to get the index of an element in an array
-- from module Data.List import function transpose
-- to get the transpose of a matrix

-- create a function to convert maybe int to int
eliminate :: Maybe Int -> Int
eliminate (Just a)=a
-- convert just int to int

-- create a function to get the index of the minimum of the array
minIndex :: [Int] -> Int
minIndex list=head (filter ((== minimum list) . (list !!)) [0..])
-- filter the array to get only places when minimum appears and save the index
-- return the head of the new array

-- create a function to get the most common bit from  a bit number
minimumArrayIndexForMatrices :: [Int] -> [[[Int]]] -> [Int]
minimumArrayIndexForMatrices array matrices=[minimum [maximum (map (eliminate . (`elemIndex` array)) y) | y <- x] | x <- matrices]
-- loop the array with matrices
-- and than loop rows of the matrices
-- after this aply eliminate and elemIndex to all elements
-- to know when element appears in the array
-- calculate the maximum of each row
-- and after this calculate the minimum for each matrices

-- create the main function
main :: IO()
main=do
  let array=[7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
  -- set the input array

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
  -- set the input matrices

  let winningMatrixIndex=minIndex (zipWith min (minimumArrayIndexForMatrices array matrices) (minimumArrayIndexForMatrices array [transpose x | x <- matrices]))
  -- get the minimum indices for input matrices and for tranpose matrices
  -- and zip them with min function
  -- calculate where the index of min is

  let finalNumber=array !! min (minimum (minimumArrayIndexForMatrices array matrices)) (minimum (minimumArrayIndexForMatrices array [transpose x | x <- matrices]))
  -- get the minimum indices for input matrices and for tranpose matrices
  -- calculate the mimimum value for each array of indexes
  -- and then calculate the min of these minimums
  -- and save the number that is in array at this position

  let remainingSum=sum ([y | x <- matrices !! winningMatrixIndex, y <- x] \\ take (eliminate (elemIndex finalNumber array)+1) array)
  -- loop the winning matrix 
  -- loop each row of it to flatten rows in a single array
  -- find the index of the finalNumber in the array with number add 1
  -- and eliminate the just from it
  -- now take first n(number calculated above) elements of array with numbers
  -- perform the difference between the elements of the matrix
  -- and the winning number to get the numbers which don't appears yet
  -- and calculate the sum of these numbers

  print (remainingSum * finalNumber)
  -- multiply the sum of remaining elements with the final number
  -- and print the result