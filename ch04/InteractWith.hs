-- file: ch04/InteractWith.hs
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where   mainWith function = do
                args <- getArgs
                case args of
                    [input, output] -> interactWith function input output
                    _ -> putStrLn "error: exactly two arguments needed"
            myFunction = fixLines

fixLines :: String -> String
fixLines s = unlines (splitLines s)

splitLines :: String -> [String]
splitLines [] = []
splitLines cs = 
    let (pre, suf) = break isNewLine cs
    in pre : case suf of
        ('\r':'\n':more) -> splitLines more
        ('\r':more) -> splitLines more
        ('\n':more) -> splitLines more
        _ -> []
    where isNewLine c = c == '\r' || c == '\n'
