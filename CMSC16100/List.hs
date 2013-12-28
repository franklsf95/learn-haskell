--file: 16100/List.hs

sum_sq_odd [] = 0
sum_sq_odd (x:xs)
    | odd x     = x ^ 2 + sum_sq_odd xs
    | otherwise = sum_sq_odd xs

sum_sq_odd' = sum . map (^2) . filter odd

product0 []     = 1
product0 (x:xs) = x * product0 xs

product1 = foldr (*) 1

map0 _ [] = []
map0 f (x:xs) = f x : map0 f xs

map1 f = foldr f' []
    where f' x xs = f x : xs

gougu n = [ (a, b, c) |
    a <- [1..n],
    b <- [a + 1 .. n],
    gcd a b == 1,
    c <- [b + 1 .. n],
    a * a + b * b == c * c ]

gcd1 x 0 = x
gcd1 0 y = y
gcd1 x y | x > y     = gcd1 y (mod x y)
         | otherwise = gcd1 x (mod y x) 

