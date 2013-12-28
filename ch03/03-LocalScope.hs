-- file: 03-LocalScope.hs

-- Lending
lend amount balance =   let reserve = 100
                            newBalance = balance - amount
                        in  if newBalance < reserve
                            then Nothing
                            else Just newBalance

lend2 amount balance =  if amount < reserve * 0.5
                        then Just newBalance
                        else Nothing
    where   reserve = 100
            newBalance = balance - amount

lend3 amount balance
    | amount <= 0            = Nothing
    | amount > reserve * 0.5 = Nothing
    | otherwise              = Just newBalance
    where reserve = 100
          newBalance = balance - amount

-- Nested Lets
foo =   let a = 1
        in  let b = 2
            in a + b
bar =    let x = 1
            in (let x = 'a'
                in x
            , x)
quux a =  let a = "foo"
          in a ++ "eek!"

-- local functions
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where   plural 0 = "no " ++ word ++ "s"
            plural 1 = "one " ++ word
            plural n = show n ++ " " ++ word ++ "s"

-- fromMaybe
fromMaybe def wrapped =
    case wrapped of
        Nothing -> def
        Just x  -> x

-- myDrop from ch02: drop the first n elements
myDrop n xs =   if n <= 0 || null xs
                then xs
                else myDrop (n - 1) (tail xs)

drop2 n xs
    | n <= 0     = xs
    | null xs    = xs
    | otherwise  = drop2 (n - 1) (tail xs)

drop3 n xs | n <= 0 = xs
drop3 _ []          = []
drop3 n (_:xs)      = drop3 (n - 1) xs

