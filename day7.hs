-- create the main function
main :: IO ()
main=do
  let array=[16,1,2,0,4,2,7,1,2,14]
  -- set the input array

  print (minimum [sum (map abs (zipWith (-) array [x | _ <- [0..length array]])) | x <- [minimum array..maximum array]])
  -- and print the result