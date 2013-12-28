--file: 16100/Type.hs

data Dollar = Dollar Double
data Euro   = Euro Double

d2e (Dollar x) = Euro (0.6938 * x)

data Complex = Complex Double Double
    deriving (Show, Eq)

instance Num Complex where
    Complex a b + Complex c d = Complex (a + c) (b + d)
    Complex a b * Complex c d = Complex (a * c) (b * d)
    abs (Complex a b) = Complex (sqrt (a * a + b * b)) 0
    signum (Complex a b) = Complex (a / c) (b / c)
        where c = sqrt (a * a + b * b)
