-- create the main function
main :: IO()
main = do
  let array=[199,200,208,210,200,207,240,269,260,263]
  -- set the input array

  print (length (filter (< 0) (zipWith (-) (init array) (tail array))))
  -- create 2 arrays
  -- first without last element and second without first element
  -- subtract second array from first array
  -- and filter the values which are bigger than zero
  -- if the value is bigger than zero, the depth is decreasing, not increasing
  -- and calculate the length of the filter array
  -- and print the result