-- file: 03-Exercises.hs
import Data.List

-- Ex.01
-- Ex.02
len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + (len xs)

-- Ex.03
mean :: [Double] -> Double
mean xs = (sum xs) / (fromIntegral (length xs))

-- Ex.04
palindromize :: [a] -> [a]
palindromize [] = []
palindromize (x:xs) = [x] ++ palindromize xs ++ [x]

-- Ex.05
isPalindrome :: [Char] -> Bool
isPalindrome xs = (xs == rev xs) && even (length xs)
    where   rev []     = []
            rev (x:xs) = rev xs ++ [x]
    -- this rev function is the same as Data.List#reverse

isPali2 :: [Char] -> Bool
isPali2 [] = True
isPali2 (x:xs) = (x == last xs) && (isPali2 (init xs))

-- Ex.06
sortByLen :: [[a]] -> [[a]]
sortByLen xs = sortBy cmp xs
    where cmp x y = compare (length x) (length y)

-- Ex.07
-- Ex.08
intersp :: a -> [[a]] -> [a]
intersp _ [] = []
intersp _ [x] = x
intersp i (x:xs) = x ++ [i] ++ intersp i xs

-- Ex.09
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ x y) = 1 + max (height x) (height y)
    --where max x y = if x > y then x else y

-- Ex.10
data Direction = LeftTurn | RightTurn | Straight
    deriving (Show)

-- Ex.11
data Point = Point {
                x :: Double,
                y :: Double
} deriving (Show)

turn :: Point -> Point -> Point -> Direction
turn a b c | slopeDiff < 0 = LeftTurn
           | slopeDiff > 0 = RightTurn
           | slopeDiff == 0 = Straight  -- should use "otherwise"
    where slopeDiff = (x b - x a) * (y c - y a) - (y b - y a) * (x c - x a)
p1 = Point 3 0
p2 = Point 0 0
p3 = Point 0 5
p4 = Point 3 4
p5 = Point 5 (-2)

-- Ex.12
directions :: [Point] -> [Direction]
directions [a, b, c]  = [turn a b c]
directions (x:y:z:xs) = directions [x,y,z] ++ directions (y:z:xs)
--directions ps | length ps < 3 = []
--directions (x:y:z:ps) = turn x y z : directions (y:z:ps)




