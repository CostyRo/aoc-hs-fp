{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.List ((\\),elemIndex,transpose)
import Data.Maybe (fromMaybe)

min_index :: Ord b => [b] -> Int
min_index list=
  head $
    filter
      ((== minimum list) . (list !!))
      [0..]

last_number_index :: Eq a => [a] -> [[a]] -> [Int]
last_number_index array matrix=[
    maximum $
      map
        (fromMaybe 0 . (`elemIndex` array))
        row
          | row <- matrix
  ]
-- calculate the index of the last number that appears in the array for each row

min_winning_index :: Eq a => [a] -> [[[a]]] -> [Int]
min_winning_index array matrices=[
    minimum $
      last_number_index
        array
        matrix
          | matrix <- matrices
  ]

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

  let transposed_matrices=[
          transpose x
            | x <- matrices
        ]

  let (min_winning_index_matrices,min_winning_index_transposed)=
        (min_win_index_mat matrices,
          min_win_index_mat transposed_matrices)
        where min_win_index_mat=min_winning_index array

  let index_winning_matrix=
        min_index $
          zipWith
            min
            min_winning_index_matrices
            min_winning_index_transposed
  -- zip minimum indices of both type of matrices with min function
  -- calculate where the index of min is

  let final_number=array !! min (minimum min_winning_index_matrices) (minimum min_winning_index_transposed)
  -- get the winning number from the min of of mimimus of matrices and transposed matrices

  let remaining_sum=
        sum $
        (\\)
          [y | x <- matrices !! index_winning_matrix, y <- x] $
          take
            (
              ((+) . fromMaybe 0)
                (final_number `elemIndex` array)
                1
            )
            array
  -- flat the winning matrix
  -- and substract from it the list with elements that appears already in the bingo list
  -- apply sum to the final list to get the sum of remaining elements

  print $
    (*)
      remaining_sum
      final_number