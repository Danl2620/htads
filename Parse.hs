
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

import qualified Htads as H

-- spaces :: Parser ()
-- spaces = skipMany1 space


{- A DEF file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
defFile :: GenParser Char st [H.Object]
defFile =
    do result <- many def
       eof
       return result

def :: GenParser Char st H.Object
def =
    do defType <- word
       spaces
       defId <- word
       spaces
       obj <- case defType of
                "item" -> parseItem
                "room" -> parseRoom
       return obj

parseItem :: GenParser Char st H.Object
parseItem = do char '{'
               sepBy line spaces
               char '}'
               return []

line :: GenParser Char st [String]
line = key <- word
       spaces
       value <- many1

parseRoom :: GenParser Char st H.Object
parseRoom = undefined

--- H.ObjectItem $ H.Item defId [] [] "" [] ""
--- H.ObjectRoom $ H.Room defId "asdf" (Map.fromList [])

-- defType :: GenParser Char st [String]
-- defType = "item" <|> "room"

word :: GenParser Char st String
word = do name <- many1 letter
          return name

body :: GenParser Char st [String]
body = do spaces
          char '{'
          spaces
          char '}'
          return []

Parsestring :: Parser String
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return x

-- -- The cell either ends with a comma, indicating that 1 or more cells follow,
-- -- or it doesn't, indicating that we're at the end of the cells for this line
-- remainingCells :: GenParser Char st [String]
-- remainingCells =
--     (char ',' >> cells)            -- Found comma?  More cells coming
--     <|> (return [])                -- No comma?  Return [], no more cells

-- -- Each cell contains 0 or more characters, which must not be a comma or
-- -- EOL
-- cellContent :: GenParser Char st String
-- cellContent =
--     many (noneOf ",\n")


-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'



parseDEF :: String -> Either ParseError [H.Object]
parseDEF input = parse defFile "(unknown)" input


