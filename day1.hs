main :: IO()
main = do
  let array=[199,200,208,210,200,207,240,269,260,263]
  print (length (filter (< 0) (zipWith (-) (init array) (tail array))))