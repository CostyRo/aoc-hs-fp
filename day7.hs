-- create the main function
main :: IO ()
main=do
  let array=[16,1,2,0,4,2,7,1,2,14]
  -- set the input array

  print (minimum [sum (map abs (zipWith (-) array [x | _ <- [0..length array]])) | x <- [minimum array..maximum array]])
  -- generate a arayy with all posible location for crabs
  -- zip the input array and the array with new location with minus function
  -- and apply abs to these elements
  -- sum all the array
  -- and get the minimum value of the array
  -- with fuel consumated for each position
  -- and print the result