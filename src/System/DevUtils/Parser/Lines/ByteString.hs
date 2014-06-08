module System.DevUtils.Parser.Lines.ByteString (
 defaultLines,
 runLines,
 weirdLines
) where

-- runLines defaultLines "#a\r\nb\rc\nd\r\ne\ndkgodkgsdo"
-- runLines weirdLines "0.1>.{....}3.4"

import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString.Char8 as B

import Data.Maybe

type Line = B.ByteString
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

item :: St B.ByteString
item = do
 st <- getState
 it <- (try (manyTill (_ident st) (try $ _eol st))
       <|> try (many1 (_ident st))
       <?> "item")
 return $ B.pack it

line :: St (Maybe B.ByteString)
line = do
 do {
  try (comment >> return Nothing)
  <|> try (item >>= \s -> case (B.length s) of
   0 -> return Nothing
   _ -> return $ Just s)
  }

parseLines :: St [B.ByteString]
parseLines = do
 linelist <- many line
       <?> "line"
 return $ catMaybes linelist

runLines' :: Lines -> St [B.ByteString] -> B.ByteString -> Either String [B.ByteString]
runLines' lineInfo p input = do
 case (runParser p lineInfo "Lines" input) of
  Left err -> Left $ "Parser error: " ++ show err
  Right val -> Right val

runLines :: Lines -> B.ByteString -> Either String [B.ByteString]
runLines lineInfo input = runLines' lineInfo parseLines input
