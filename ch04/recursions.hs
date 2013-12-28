-- file: ch04/recursions.hs
import Data.Char (digitToInt)

asInt :: String -> Int
asInt xs = asIntHelper 0 xs
    where   asIntHelper acc (x:xs) = asIntHelper acc' xs
                where   acc' = acc * 10 + digitToInt x
            asIntHelper acc _      = acc

s = [1, 1, 2, 3, 5, 8, 13, 21, 34]

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap _ _      = []

foldMap :: (a -> b) -> [a] -> [b]
foldMap f xs = foldr step [] xs
    where step x acc = f x : acc

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f (x:xs) | f x       = x : myFilter f xs
                  | otherwise = myFilter f xs
myFilter _ _ = []

foldFilter :: (a -> Bool) -> [a] -> [a]
foldFilter f xs = foldr step [] xs
    where step x acc | f x       = x : acc
                     | otherwise = acc
foldlFilter f xs = foldl step [] xs
    where step acc x | f x       = acc ++ [x]
                     | otherwise = acc

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs
myFoldl _ acc []     = acc

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)
myFoldr _ acc []     = acc

niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs

identity :: [a] -> [a]
identity xs = (:) [] xs





