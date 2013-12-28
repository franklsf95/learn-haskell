-- file: ch04/functionsExercise.hs
-- Ex.01
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs   -- a different way!

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- generalized solution
safeVariantOf :: ([a] -> b) -> ([a] -> Maybe b)
safeVariantOf _ [] = Nothing
safeVariantOf f xs = Just (f xs)


-- Ex.02
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = [[]]
splitWith p (x:xs) | not (p x) = splitWith p xs
splitWith p xs = takeWhile p xs : splitWith p rest
    where   rest = dropWhile p xs


-- Ex.04
s = "Hello\nWorld\nChina"
transpo :: String -> String
transpo "" = ""
transpo cs = unlines (transposeLines (lines cs))

transposeLines :: [String] -> [String]
transposeLines (x:xs) = transposeHelper acc0 xs
    where   acc0 = map char2str x
            char2str c = [c]

transposeHelper :: [String] -> [String] -> [String]
transposeHelper acc (x:xs) = transposeHelper acc' xs
    where acc' = zipWith append x acc
transposeHelper acc _      = acc

append :: Char -> String -> String
append c s = s ++ [c]


