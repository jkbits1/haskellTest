-- conversion of either example

divSafe2 :: (Integral a) => a -> [a] -> Maybe [a]

divSafe2 _ [] = Just []
divSafe2 _ (0: xs) = Nothing
divSafe2 num (denom : xs) =
  let 
    ans = divSafe2 num xs
  in
    case ans of
      Nothing    -> Nothing
      Just res -> Just $ (div num denom) : res

main :: IO ()
main = 
  print $ divSafe2 10 [2,5,0]

-- divSafe2 10 [2,5,0]

-- from rwh
mbDef :: b -> (a -> b) -> Maybe a -> b
mbDef n _ Nothing = n
mbDef _ f (Just x) = f x
