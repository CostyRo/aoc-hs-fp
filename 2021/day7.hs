main :: IO ()
main=do
  let array=[16,1,2,0,4,2,7,1,2,14]

  print $
    minimum
      [
        sum $
          map
            abs $
            zipWith
              (-)
              array
              [x | _ <- [0..length array]]
                | x <- [minimum array..maximum array]
      ]