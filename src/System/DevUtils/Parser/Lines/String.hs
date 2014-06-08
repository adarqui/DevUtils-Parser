module System.DevUtils.Parser.Lines.String (
 defaultLines,
 runLines,
 weirdLines
) where

-- runLines defaultLines "#a\r\nb\rc\nd\r\ne\ndkgodkgsdo"
-- runLines weirdLines "0.1>.{....}3.4"

import Text.Parsec
import Text.Parsec.String

import Data.Maybe

type Line = String
type St a = GenParser Char Lines a

data Lines = Lines {
 _ident :: St Char,
 _eol :: St String,
 _comment :: St String
}

defaultLines :: Lines
defaultLines = _Lines
 where
  _Lines = Lines {
 _ident = anyToken,
 _eol = (try (string "\r\n") <|> try (string "\r") <|> try (string "\n") <?> "end of line"),
 _comment = do { (try (string "#") <|> try (string ";"));
                  (try (manyTill anyToken (_eol _Lines)) <|> (try (many anyToken)))
               }
 }

weirdLines :: Lines
weirdLines = Lines {
 _ident = anyToken,
 _eol = (try (string ".")),
 _comment = do { string "{" ;
               ; manyTill anyChar (try (string "}"))
               }
}

comment :: St ()
comment = do
 st <- getState
 _comment st
 return ()

item :: St String
item = do
 st <- getState
 it <- (try (manyTill (_ident st) (try $ _eol st))
       <|> try (many1 (_ident st))
       <?> "item")
 return it

line :: St (Maybe String)
line = do
 do {
  try (comment >> return Nothing)
  <|> try (item >>= \s -> case s of
   [] -> return Nothing
   _ -> return $ Just s)
  }

parseLines :: St [String]
parseLines = do
 linelist <- many line
       <?> "line"
 return $ catMaybes linelist

runLines' :: Lines -> St [String] -> String -> Either String [String]
runLines' lineInfo p input = do
 case (runParser p lineInfo "Lines" input) of
  Left err -> Left $ "Parser error: " ++ show err
  Right val -> Right val

runLines :: Lines -> String -> Either String [String]
runLines lineInfo input = runLines' lineInfo parseLines input
