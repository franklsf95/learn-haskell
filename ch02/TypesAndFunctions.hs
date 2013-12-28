-- file: 02-TypesAndFunctions.hs
isOdd n = mod n 2 == 1
myDrop n xs =   if n <= 0 || null xs
                then xs
                else myDrop (n - 1) (tail xs)
lastSnd xs = last (init xs)
law_of_cosines a b gamma = sqrt (a * a + b * b - 2 * a * b * cos (gamma / 180 * pi))
