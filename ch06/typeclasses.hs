-- file: ch06/typeclasses.hs
-- defining typeclasses
class BaseEq a where
  isEqual :: a -> a -> Bool
  isEqual x y = not $ isNotEqual x y

  isNotEqual :: a -> a -> Bool
  isNotEqual x y = not $ isEqual x y

instance BaseEq Bool where
  isEqual True  True  = True
  isEqual False False = True
  isEqual _ _         = False

-- implementing standard typeclasses
data Color = Red | Green | Blue

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

instance Show Color where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"

instance Read Color where
  readsPrec _ input =
    tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
    where tryParse [] = []
          tryParse ((key, dat):xs) =
            if (take klen trimmed) == key
              then [(dat, drop klen trimmed)]
              else tryParse xs
            where klen = length key
                  trimmed = trim input

trim :: String -> String
trim (' ':xs) = trim xs
trim xs = xs
