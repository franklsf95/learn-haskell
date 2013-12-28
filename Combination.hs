perm :: Int -> [a] -> [[a]]
perm m l = auxPerm l (length l) m [] []

deleteAt :: Int -> [a] -> [a]
deleteAt i l = (take i l) ++ (drop (i+1) l)

auxPerm :: [a] -> Int -> Int -> [a] -> [[a]] -> [[a]]
auxPerm _ _ 0 current acc = current:acc
auxPerm _ 0 _ _       acc = error "Not enough elements"
auxPerm l n m current acc = foldl f acc [0..(length l)]
    where f acc i = auxPerm (deleteAt i l) (n - 1) (m - 1) ((l !! i):current) acc

comb :: Int -> [a] -> [[a]]
comb m l = auxComb l (length l) m [] []

auxComb :: [a] -> Int -> Int -> [a] -> [[a]] -> [[a]]
auxComb _      _ 0 current acc = current:acc
auxComb _      0 _ _       acc = acc
auxComb (x:xs) n m current acc = f True ++ f False
    where f t = auxComb xs (n - 1) (if t then m - 1 else m)
                        (if t then x:current else current) acc

subsets :: [a] -> [[a]]
subsets l = foldl (\acc m -> (comb m l) ++ acc) [] [1..(length l + 1)]
