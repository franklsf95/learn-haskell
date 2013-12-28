-- file: ch05/Prettify.hs
module Prettify where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
x <> Empty = x
Empty <> y = y
x <> y = Concat x y

hcat :: [Doc] -> Doc
hcat = foldr (<>) empty

fsep :: [Doc] -> Doc
fsep = foldr (</>) empty

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _ ) = flatten x
flatten other          = other

compact :: Doc -> String
compact x = transform [x]
      where transform [] = ""
            transform (d:ds) = case d of
                                   Empty     -> transform ds
                                   Char c    -> c : transform ds
                                   Text s    -> s ++ transform ds
                                   Line      -> '\n' : transform ds
                                   a `Concat` b -> transform (a:b:ds)
                                   _ `Union` b  -> transform (b:ds)

