-- file: ch06/read.hs
main = do
  putStrLn "Please enter a Double:"
  input <- getLine
  let d = (read input) :: Double
  putStrLn $ "Twice " ++ show d ++ " is " ++ show (d * 2)
