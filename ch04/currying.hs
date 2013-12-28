-- file: ch04/currying.hs
import Data.List
import Data.Char

nicerSum :: [Integer] -> Integer
nicerSum = foldr (+) 0

-- @
suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _          = []

suffixes2 (x:xs) = (x:xs) : suffixes2 xs
suffixes2 _      = []

suffixes3 xs = init (tails xs)

-- (.) function
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

suffixes4 = compose init tails

suffixes5 = init . tails

capCount :: String -> Int
capCount0 xs = length (filter (\x -> isUpper (head x)) (words xs))
capCount = length . filter (isUpper . head) . words

dlts :: String -> [String]


