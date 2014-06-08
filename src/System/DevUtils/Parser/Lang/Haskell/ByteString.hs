{-# LANGUAGE OverloadedStrings #-}
module System.DevUtils.Parser.Lang.Haskell.ByteString (
 lexer,
 identifier,
 reserved,
 operator,
 reservedOp,
 charLiteral,
 stringLiteral,
 natural,
 integer,
 float,
 naturalOrFloat,
 decimal,
 hexadecimal,
 octal,
 symbol,
 lexeme,
 whiteSpace,
 parens,
 braces,
 angles,
 brackets,
 squares,
 semi,
 comma,
 colon,
 dot,
 semiSep,
 semiSep1,
 commaSep,
 commaSep1
) where

import Control.Monad.Identity

import Text.Parsec
import Text.Parsec.Language (haskellDef, emptyDef)
import qualified Text.Parsec.Token as PT

import qualified Text.Parsec.ByteString as PB
import qualified Data.ByteString.Char8 as B

haskellStyle :: PT.GenLanguageDef B.ByteString () Identity
haskellStyle = emptyDef
 { PT.commentStart   = "{-"
 , PT.commentEnd     = "-}"
 , PT.commentLine    = "--"
 , PT.nestedComments = True
 , PT.identStart     = letter
 , PT.identLetter    = alphaNum <|> oneOf "_'"
 , PT.opStart        = PT.opLetter haskellStyle
 , PT.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
 , PT.reservedOpNames= []
 , PT.reservedNames  = []
 , PT.caseSensitive  = True
 }

lexer :: PT.GenTokenParser B.ByteString () Identity
lexer = PT.makeTokenParser haskellStyle

identifier = PT.identifier lexer
reserved = PT.reserved lexer
operator = PT.operator lexer
reservedOp = PT.reservedOp lexer
charLiteral = PT.charLiteral lexer
stringLiteral = PT.stringLiteral lexer
natural = PT.natural lexer
integer = PT.integer lexer
float = PT.float lexer
naturalOrFloat = PT.naturalOrFloat lexer
decimal = PT.decimal lexer
hexadecimal = PT.hexadecimal lexer
octal = PT.octal lexer
symbol = PT.symbol lexer
lexeme = PT.lexeme lexer
whiteSpace = PT.whiteSpace lexer
parens = PT.parens lexer
braces = PT.braces lexer
angles = PT.angles lexer
brackets = PT.brackets lexer
squares = PT.squares lexer
semi = PT.semi lexer
comma = PT.comma lexer
colon = PT.colon lexer
dot = PT.dot lexer
semiSep = PT.semiSep lexer
semiSep1 = PT.semiSep1 lexer
commaSep = PT.commaSep lexer
commaSep1 = PT.commaSep1 lexer
