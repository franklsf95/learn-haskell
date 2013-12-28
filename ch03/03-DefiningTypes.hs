-- file: 03-DefiningTypes.hs
data BookInfo = Book { espn      :: Integer
                     , title     :: String
                     , author    :: [String]
} deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody
                  deriving (Show)
type Review = (BookInfo, Int, String)

b1 = Book 978 "Programming" ["Frank", "Luan"]

data Color = Red | Yellow | Blue
             | Color Int
             deriving (Eq, Show)

myNot True = False
myNot False = True

sumList (x:xs) = x + sumList xs
sumList _      = 0

three (3:xs) = xs
complicated (True, a, xs, 5) = (a, tail xs)

-- Maybe
data Perhaps a = Only a | NotAtAll
x = Only True
y = NotAtAll

-- List
data Li a = C a (Li a)
          | Nil
            deriving (Show)
list2Li (x:xs) = C x (list2Li xs)
list2Li []     = Nil
li2List (C x y) = x : li2List y
li2List Nil     = []

-- Tree
data Tree a = Node0 a (Tree a) (Tree a)
            | Empty
              deriving (Show)
simpleTree = Node0 "parent" (Node0 "left child"  Empty Empty)
                            (Node0 "right child" Empty Empty)

data Tree2 a = Node a (Maybe (Tree2 a)) (Maybe (Tree2 a))
               deriving (Show)

tree = Node "left" Nothing Nothing
tree2 = Node "parent" (Just tree) Nothing

same (Node a _ _) (Node b _ _) | a == b
    = Just a
same _ _ = Nothing

-- Second
second :: [a] -> a
second xs = if null (tail xs)
            then error "list too short"
            else head (tail xs)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))

tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing

