module Alias (parseAliases) where 

import Control.Monad
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

aliasFile = do spaces
               endBy line eol
-- line = liftM2 (,) (cmdContents <* spaces) (char '=' >> (bindingContents <* spaces))

line = do cmd <- cmdContents
          spaces
          char '='
          spaces
          binding <- bindingContents
          return (cmd, binding)

cmdContents = many (noneOf "= ,\n\r")
-- cmdContents = sepBy cell (char '=')
bindingContents = quotedCell <|> many (noneOf "=,\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseAliases :: String -> Either ParseError [(String, String)]
parseAliases input = parse aliasFile "alias" input

main =
    do c <- getContents
       print $ case parseAliases c of
                 Left e -> "Error parsing input:" ++ show e
                 Right r -> show $ Map.fromList r

-- $ case parse csvFile "alias" c of
