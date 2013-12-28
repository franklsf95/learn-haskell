--file: 16100-1/Natural.hs

data NaturalNumber = Zero
                   | S NaturalNumber
    deriving (Show)

instance Eq NaturalNumber where
  Zero == Zero = True
  Zero == S _  = False
  S _  == Zero = False
  S x  == S y  = x == y

instance Ord NaturalNumber where
  compare Zero Zero  = EQ
  compare Zero (S _) = LT
  compare (S _) Zero = GT
  compare (S x) (S y) = compare x y

instance Num NaturalNumber where
  x + Zero = x
  x + S y  = S (x + y)

  x * Zero = Zero
  x * S y  = x * y + x

  fromInteger 0 = Zero
  fromInteger n = S (fromInteger (n - 1))

  abs x = x

  signum Zero  = Zero
  signum (S _) = S Zero

nat x = x + Zero

zero  = Zero
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven
nine  = S eight
ten   = S nine

inf   = S inf
