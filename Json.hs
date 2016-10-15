{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Json
       (
         JValue(..)
       , JAry(..)
       , JObj(..)
       ) where

import Control.Arrow (second)
import ApplicativeParsec
import qualified Numeric as N

newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)

newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
              deriving (Eq, Ord, Show)

type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
    toJValue                    = JBool
    fromJValue (JBool b)        = Right b
    fromJValue _                = Left "not a JSON boolean"

instance JSON String where
    toJValue                    = JString
    fromJValue (JString s)      = Right s
    fromJValue _                = Left "not a JSON string"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id


whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

instance (JSON a) => JSON (JAry a) where
    toJValue = JArray . JAry . map toJValue . fromJAry
    fromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
    fromJValue _ = Left "not a JSON array"

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj
    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
        where unwrap (k, v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"


p_text :: CharParser () JValue
p_text = space *> text <?> "JSON text"
    where text = JObject <$> p_object
             <|> JArray <$> p_array

p_series :: Char -> CharParser () a -> Char -> CharParser () [a]
p_series left parser right =
    between (char left <* spaces) (char right) $
            (parser <* spaces) `sepBy` (char ',' <* spaces)

p_object :: CharParser () (JObj JValue)
p_object = JObj <$> p_series '{' p_field '}'
    where p_field = (,) <$> (p_string <* char ':' <* spaces) <*> p_value

p_array :: CharParser () (JAry JValue)
p_array = JAry <$> p_series '[' p_value ']'

p_value :: CharParser () JValue
p_value = value <* spaces
    where value =     JString <$> p_string
                  <|> JNumber <$> p_number
                  <|> JObject <$> p_object
                  <|> JArray <$> p_array
                  <|> JBool <$> p_bool
                  <|> JNull <$ string "null"
                  <?> "JSON value"

p_bool :: CharParser () Bool
p_bool =     True <$ string "true"
         <|> False <$ string "false"


p_number :: CharParser () Double
p_number = do s <- getInput
              case N.readSigned N.readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty

p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> (p_escape <|> p_unicode)
                  <|> satisfy (`notElem` "\"\\")

p_escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode c r = r <$ char c

p_unicode :: CharParser () Char
p_unicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
              where ((code,_):_) = N.readHex x

-- result :: JValue
-- result = JObject . JObj $ [
--   ("query", JString "awkward squad haskell"),
--   ("estimatedCount", JNumber 3920),
--   ("moreResults", JBool True),
--   ("results", JArray . JAry $ [
--      JObject . JObj $ [
--       ("title", JString "Simon Peyton Jones: papers"),
--       ("snippet", JString "Tackling the awkward ..."),
--       ("url", JString "http://.../marktoberdorf/")
--      ]])
--   ]


text = " { \"query\": \"awkward squad haskell\", \"estimatedCount\": 3920, \"moreResults\": true, \"results\": [{  \"title\": \"Simon Peyton Jones: papers\",  \"snippet\": \"Tackling the awkward squad: monadic input/output ...\",  \"url\": \"http://research.microsoft.com/~simonpj/papers/marktoberdorf/\"  },  {  \"title\": \"Haskell for C Programmers | Lambda the Ultimate\",  \"snippet\": \"... the best job of all the tutorials I've read ...\",  \"url\": \"http://lambda-the-ultimate.org/node/724\" } ] }"


main = parse p_text "(json)" text
