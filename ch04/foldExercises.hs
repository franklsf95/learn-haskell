-- file: ch04/foldExercises.hs
import Data.Char (digitToInt)

-- Ex.01
asInt :: String -> Int
asInt "" = error "Contains no digit" -- Ex.03
asInt ('-':xs) = - (asInt xs)        -- Ex.02
asInt xs = foldl step 0 xs
    where step acc x = acc * 10 + digitToInt x

-- Ex.05/06
concatl :: [[a]] -> [a]
concatl xs = foldl (++) [] xs

concatr :: [[a]] -> [a]
concatr xs = foldr (++) [] xs

-- Ex.07
-- takeWhile is NOT filter! Stop when first unmatch!
test1 = [1,3,5,7,8,9,11]
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pr (x:xs) | pr x = x : myTakeWhile pr xs
                      | otherwise = []
myTakeWhile _  [] = []

takeWhileR pr xs = foldr step [] xs
    where step x acc | pr x = x : acc
                     | otherwise = []

-- Ex.08/09
test2 = [4,3,1,2,5,0,9,5,8]
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy cmp xs = foldl step [] xs
      where step []  x = [[x]]
            step acc x | cmp (head (last acc)) x = (init acc) ++ [((last acc) ++ [x])]
                       | otherwise = acc ++ [[x]]

-- Ex.10
any1 :: (a -> Bool) -> [a] -> Bool
any1 p xs = foldr step False xs
    where step x acc = (p x) || acc
any2 p xs = foldr (||) False (map p xs)

--words1 :: String -> [String]
--words1 xs = fst (foldr step ([], True) xs)
--      where step x (acc, True)  | x == ' '  = (acc, True)
--                                | otherwise = ([x] ++ acc, False)
--            step x (acc, False) | x == ' '  = (acc, True)
--                                | otherwise = ((x : (head acc)) : tail acc, False)

unlines1 :: [String] -> String
unlines1 xs = foldr step "" xs
      where step x acc = x ++ "\n" ++ acc
