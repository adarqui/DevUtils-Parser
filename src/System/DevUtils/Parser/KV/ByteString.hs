module System.DevUtils.Parser.KV.ByteString (
 defaultKV,
 runKV,
 weirdKV
) where

-- runKV defaultKV ";a=1\r\nb=2\rc=1\nd=2\r\ne=1\n>f=odkgodkgsdo"
-- runKV weirdKV "h>1.g>2{....}p>1"

import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString.Char8 as B
import Data.Maybe

type Pair = (B.ByteString, B.ByteString)
type St a = GenParser Char KV a

data KV = KV {
 _ident :: St String,
 _delim :: St String,
 _eol :: St String,
 _comment :: St String
}

defaultKV :: KV
defaultKV = KV {
 _ident = (many1 $ try letter <|> try digit <|> try (oneOf "_-")),
 _delim = (try (string "=") <|> try (string "->") <|> try (string ":")),
 _eol = (try (string "\r\n") <|> try (string "\r") <|> try (string "\n")),
 _comment = (try (string "#") <|> try (string ";"))
}

weirdKV :: KV
weirdKV = KV {
 _ident = (many1 $ try letter),
 _delim = (try (string ">")),
 _eol = (try (string ".")),
 _comment = do { string "{" ;
               ; manyTill anyChar (try (string "}"))
               }
}

ident :: St String
ident = do
 st <- getState
 _ident st

comment :: St ()
comment = do
 st <- getState
 _comment st
 _ <- manyTill anyChar $_eol st
 return ()

eol :: St ()
eol = do
 st <- getState
 _eol st
 return ()

item :: St (B.ByteString, B.ByteString)
item = do
 st <- getState
 key <- ident
 skipMany space
 _delim st
 skipMany space
 value <- manyTill anyChar (try eol <|> try comment <|> try eof)
 return (B.pack key, B.pack value)

line :: St (Maybe (B.ByteString, B.ByteString))
line = do
 do { try $ skipMany space ;
 try (comment >> return Nothing) <|> try (item >>= return . Just) }

parseKV :: St [Pair]
parseKV = do
 kvlines <- many line
       <?> "line"
 return $ catMaybes kvlines

runKV' :: KV -> St [Pair] -> B.ByteString -> Either String [Pair]
runKV' kv p input = do
 case (runParser p kv "KV" input) of
  Left err -> Left $ "Parser error: " ++ show err
  Right val -> Right val

runKV :: KV -> B.ByteString -> Either String [Pair]
runKV kv input = runKV' kv parseKV input
