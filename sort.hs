import Test.HUnit

-- Insertion Sort

-- Insert an element into a sorted increasing list
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x > y then y:(insert x ys) else x:y:ys

insertionSort :: (Ord a) => [a] -> [a]
insertionSort x = _insertionSort x []

_insertionSort :: (Ord a) => [a] -> [a] -> [a]
_insertionSort [] y = y
_insertionSort (x:xs) y = _insertionSort xs (insert x y)

-- Selection Sort

_minAndRest :: (Ord a) => [a] -> a -> [a] -> (a, [a])
_minAndRest [] min rest = (min, rest)
_minAndRest (x:xs) min rest = if x < min then _minAndRest xs x (min:rest)
                                         else _minAndRest xs min (x:rest)

minAndRest :: (Ord a) => [a] -> (a, [a])
minAndRest [] = error "no minimum element"
minAndRest [x] = (x, [])
minAndRest (x:xs) = _minAndRest xs x []

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort [x] = [x]
selectionSort xs = min:(selectionSort rest)
    where (min, rest) = minAndRest xs

-- Quick Sort

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort small ++ [x] ++ quickSort large
    where small = filter (<= x) xs
          large = filter (> x) xs

-- Merge Sort

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
    where n = quot (length xs) 2
          firstHalf = take n xs
          secondHalf = drop n xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x > y then y:(merge (x:xs) ys) else x:(merge xs (y:ys))

-- Tests
testFor :: (Ord a, Show a, Num a) => ([a] -> [a]) -> Test
testFor f = TestList ["Test1" ~: "short list" ~:      [1,2,3] ~=? (f [3,2,1]),
                      "Test2" ~: "empty list" ~:      [] ~=? (f []),
                      "Test3" ~: "singleton list" ~:  [5] ~=? (f [5]),
                      "Test4" ~: "reversed list" ~:   [1,2,3,5,6] ~=? (f [6,5,3,2,1]),
                      "Test5" ~: "repetitive list" ~: [1,5,6,6,8,8] ~=? (f [5,6,8,1,8,6])
                      ]

main = do
    putStrLn "Testing insertion sort"
    runTestTT (testFor insertionSort)
    putStrLn "Testing selection sort"
    runTestTT (testFor selectionSort)
    putStrLn "Testing merge sort"
    runTestTT (testFor mergeSort)
    putStrLn "Testing quick sort"
    runTestTT (testFor quickSort)



