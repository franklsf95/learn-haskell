-- file: GEB/WJU.hs
type WJU = Char

derive1 :: [WJU] -> [[WJU]]
derive1 xs | last xs == 'J' = [xs ++ "JU"]
           | otherwise      = []

derive2 :: [WJU] -> [[WJU]]
derive2 ('W':xs) = ["W" ++ xs ++ xs]
derive2 _        = []

derive3 :: [WJU] -> [[WJU]]
derive3 xs = help3 "" xs []

help3 :: [WJU] -> [WJU] -> [[WJU]] -> [[WJU]]
help3 pass ('J':'J':'J':xs) acc = help3 (pass ++ "J") ('J':'J':xs) (newstr : acc)
  where newstr = pass ++ "U" ++ xs
help3 pass (x:xs) acc = help3 (pass ++ [x]) xs acc
help3 pass "" acc     = acc

derive4 :: [WJU] -> [[WJU]]
derive4 xs = help4 "" xs []

help4 pass ('U':'U':xs) acc = help4 (pass ++ "U") ('U':xs) (newstr : acc)
  where newstr = pass ++ xs
help4 pass (x:xs) acc = help4 (pass ++ [x]) xs acc
help4 pass "" acc     = acc

derive :: [WJU] -> [[WJU]]
derive xs = derive1 xs ++ derive2 xs ++ derive3 xs ++ derive4 xs

deriveAll :: [[WJU]] -> [[WJU]]
deriveAll xs = foldr f [] xs
  where f x acc = derive x ++ acc



