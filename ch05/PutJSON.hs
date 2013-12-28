-- file: ch05/PutJSON.hs

module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"  -- downcase
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"
renderJValue (JObject o)   = "{ " ++ pairs o ++ " }"
      where pairs ps = intercalate ", " (map renderPair ps)
            renderPair (k, v) = show k ++ ": " ++ renderJValue v
renderJValue (JArray a)    = "[ " ++ intercalate ", " (map renderJValue a) ++ " ]"

putJValue :: JValue -> IO ()
putJValue = putStrLn . renderJValue
