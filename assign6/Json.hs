{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-deprecated-flags #-}

import Data.List

-- Json values
data Json
  = JSNull                      -- null
  | JSFalse                     -- false
  | JSTrue                      -- true
  | JSNumber Double             -- numbers, 123.456
  | JSString String             -- strings, "hello"
  | JSList [Json]               -- lists,   [x,y,..]
  | JSObject [(String,Json)]    -- objects, {"k":v, "k2":v2, ..}
  deriving (Eq)

-- custom pretty printer for Json values, you don't have to understand this code
instance Show Json where
  showsPrec _ = showJson 0
    where
    showJson :: Int -> Json -> ShowS
    showJson _ JSNull        = showString "null"
    showJson _ JSFalse       = showString "false"
    showJson _ JSTrue        = showString "true"
    showJson _ (JSString x)  = shows x
    showJson _ (JSNumber x)  = shows x
    showJson i (JSList xs)   = showString "[" . showList showListItem i xs . showString "]"
    showJson i (JSObject xs) = showString "{" . showList showObjectItem i xs . showString "}"

    showIndent :: Int -> ShowS
    showIndent i = showString (replicate (2*i) ' ')

    showListItem :: Int -> Json -> ShowS
    showListItem i v = showIndent i . showJson i v

    showObjectItem :: Int -> (String,Json) -> ShowS
    showObjectItem i (n, v) = showIndent i . shows n . showString ": " . showJson i v

    showList :: (Int -> a -> ShowS) -> Int -> [a] -> ShowS
    showList _        _ [] = id
    showList showItem i xs = showString "\n"
                           . foldr (.) id (intersperse (showString ",\n") (map (showItem (i+1)) xs))
                           . showString "\n" . showIndent i

-- Person data type
data Person = Person { name :: String, age :: Double, knowsFP :: Bool }
  deriving (Eq)

-- Converting Haskell values to Json
class ToJson a where
    toJson :: a -> Json

instance ToJson () where
  toJson () = JSNull

instance ToJson Bool where
  toJson :: Bool -> Json
  toJson True = JSTrue
  toJson False = JSFalse

instance ToJson Double where
  toJson = JSNumber

instance ToJson String where
  toJson = JSString

instance ToJson a => ToJson [a] where
    toJson [] = JSList []  -- Represent an empty list as an empty JSON array
    toJson (x:xs) = JSList (toJson x : map toJson xs)

instance (ToJson a, ToJson b) => ToJson (a, b) where
    toJson (x, y) = JSObject [("first", toJson x), ("second", toJson y)]

-- instance ToJson Person where
-- Optional extra: instance ToJson Int


-- Converting from Json back to Haskell values

-- class FromJson
-- instance FromJson ()
-- instance FromJson Bool
-- instance FromJson Double
-- instance FromJson String
-- instance .. => FromJson (a,b)
-- instance FromJson Person
-- Optional extra: instance .. => FromJson [a]
-- Optional extra: instance FromJson Int


-- Test cases

person1, person2 :: Person
person1 = Person {name="Twan", age=38, knowsFP=True}
person2 = Person {name="Wim-Lex", age=56, knowsFP=False}

persons :: [Person]
persons = [person1, person2]

tuple1 :: ((), (Bool,Bool))
tuple1 = ((), (True, False))

json1, json2, json3, json4 :: Json
json1 = JSObject [("name",JSString "Twan"),("age",JSNumber 38.0),("knowsFP",JSTrue)]
json2 = JSObject [("name",JSString "Wim-Lex"),("age",JSNumber 56.0),("knowsFP",JSFalse)]
json3 = JSList [json1, json2]
json4 = JSList [JSNull, JSList [JSTrue, JSFalse]]

{-
testToJson :: Bool
testToJson = toJson person1 == json1 && 
             toJson person2 == json2 &&
             toJson persons == json3 &&
             toJson tuple1 == json4
-}

{-
testFromJson :: Bool
testFromJson = Just person1 == fromJson json1 && 
               Just person2 == fromJson json2 &&
               Just tuple1 == fromJson json4 &&
               fromJson json1 == (Nothing :: Maybe String) &&
               fromJson json1 == (Nothing :: Maybe Double) &&
               fromJson json1 == (Nothing :: Maybe Bool)
-}

