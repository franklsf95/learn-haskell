import Data.Char

data Operator = Plus | Minus | Multiply | Divide
data ETree = Leaf Double | Tree Operator ETree ETree

priority :: Operator -> Int
priority Plus     = 1
priority Minus    = 1
priority Multiply = 2
priority Divide   = 2

apply :: Operator -> Double -> Double -> Double
apply Plus     = (+)
apply Minus    = (-)
apply Multiply = (*)
apply Divide   = (/)

operatorByChar :: Char -> Operator
operatorByChar '+' = Plus
operatorByChar '-' = Minus
operatorByChar '*' = Multiply
operatorByChar '/' = Divide

instance Show Operator where
  show Plus     = "+"
  show Minus    = "-"
  show Multiply = "*"
  show Divide   = "/"

-- Prefix notation by default
instance Show ETree where
  show (Leaf x) = show x
  show (Tree f a b) = show f ++ " " ++ show a ++ " " ++ show b

showInfix :: ETree -> String
showInfix (Leaf x) = show x
showInfix (Tree f a b) = showInfixHelper p a ++ show f ++ showInfixHelper p b
  where p = priority f

showInfixHelper :: Int -> ETree -> String
showInfixHelper _ (Leaf x) = show x
showInfixHelper p t@(Tree f a b)
    | p > priority f =  "(" ++ showInfixHelper 0 t ++ ")"
    | otherwise      = showInfix t

-- 5*(3+2)-9/(4-1) = 25 - 3
t0 = Tree Minus (Tree Multiply (Leaf 5) (Tree Plus  (Leaf 3) (Leaf 2)))
                (Tree Divide   (Leaf 9) (Tree Minus (Leaf 4) (Leaf 1)))

-- (6*(1+2)-3))*(5-2)
t1 = Tree Multiply (Tree Multiply (Leaf 6)
                                  (Tree Minus (Tree Plus (Leaf 1) (Leaf 2) )
                                              (Leaf 3)))
                   (Tree Minus (Leaf 5) (Leaf 2))


eval :: ETree -> Double
eval (Leaf x) = x
eval (Tree f a b) = (apply f) (eval a) (eval b)



