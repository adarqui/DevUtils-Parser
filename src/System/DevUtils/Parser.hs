module System.DevUtils.Parser (
 parseUrl,
 runUrl,
) where

import Text.Parsec
import Text.Parsec.String

import qualified Control.Applicative as APP

import qualified System.DevUtils.Redis as R
import qualified System.DevUtils.Ssh as S
import qualified System.DevUtils.Auth as A
import qualified System.DevUtils.Connection as C
import System.DevUtils.Url


type St a = GenParser Char Url a


password = between (char '(') (char ')') (many1 $ noneOf "()")
field = many1 $ noneOf ": "
port = do
 s <- try (many1 digit) <?> "digits"
 let i = read s :: Integer
 if (0 >= i || i > 65535) then return "port too large" else return s


{-
 - auth://user
 - auth://user:(pass)
 -}

parseUrlAuth' :: St Url
parseUrlAuth' = do
 _ <- string "auth://"
 parseUrlAuth


{-
 - user
 - user:(pass)
 -}

parseUrlAuth :: St Url
parseUrlAuth = do
 user <- field
 pass <- try $ optionMaybe $ (string ":" >> password)
 (putState $ UrlAuth A.Auth { A._user = user, A._pass = pass }) >> getState >>= return


-- con:// wrapper
parseUrlConnectionWrapper :: String -> C.ConnectionType -> St Url
parseUrlConnectionWrapper prefix conType = do
 _ <- string $ prefix ++ "://"
 _ <- parseUrlConnection
 modifyState (\(UrlConnection x) -> UrlConnection x { C._type = conType })
 getState >>= return

-- tcp://
parseUrlConnectionTCP :: St Url
parseUrlConnectionTCP = parseUrlConnectionWrapper "tcp" C.TCP

-- udp://
parseUrlConnectionUDP :: St Url
parseUrlConnectionUDP = parseUrlConnectionWrapper "udp" C.UDP

-- unix://
parseUrlConnectionUNIX :: St Url
parseUrlConnectionUNIX = parseUrlConnectionWrapper "unix" C.UNIX


{-
 - con://unix
 - con://host:port
 -}

parseUrlConnection' :: St Url
parseUrlConnection' = do
 _ <- string "con://"
 parseUrlConnection


{-
 - file
 - host:port
 -}

parseUrlConnection :: St Url
parseUrlConnection = do
 dest <- field
 maybePort <- try $ optionMaybe $ (string ":" >> port)
 (putState $ UrlConnection C.Connection { C._dest = dest , C._port = (maybe 0 (\x -> read x :: Integer) maybePort), C._type = C.UNKNOWN }) >> getState >>= return


{-
 - redis://host
 - redis://host:port
 - redis://host:port/options
 - redis://host/options
 - redis://user:(pass)@host
 - redis://user:(pass)@host/options
 - etc.
 -}
parseUrlRedis' :: St Url
parseUrlRedis' = do
 _ <- string "redis://"
 parseUrlRedis

parseUrlRedis :: St Url
parseUrlRedis = do
 putState $ UrlRedis R.defaultRedis
 auth <- optionMaybe (try parseUrlAuth)
 _ <- case auth of
  Just (UrlAuth val) -> do
   _ <- char '@'
   (putState $ UrlRedis R.Redis { R._auth = Just val }) >> getState >>= return
  _ -> do
   return UrlNone
 (UrlRedis st) <- getState
 (Just (UrlConnection con)) <- optionMaybe (parseUrlConnection)
 (putState $ UrlRedis st { R._con = con }) >> getState >>= return


{-
 - should make redis:// ssh:// ftp:// etc all use one parser, and then just parse the additional options for the specific type of connection
parseUrlSsh :: Parser Url
parseUrlSsh = do
 string "ssh://"
 return $ UrlSsh { }
-}

parseUrl :: St Url
parseUrl = do
 (try parseUrlAuth')
 <|> (try parseUrlRedis')
 <|> (try parseUrlConnection')
 <|> (try parseUrlConnectionTCP)
 <|> (try parseUrlConnectionUDP)
 <|> (try parseUrlConnectionUNIX)
 <?> "url"

runUrl' :: St Url -> String -> Either String Url
runUrl' p input = do
 case (runParser p UrlNone "Url" input) of
  Left err -> Left $ "Parse error: " ++ show err
  Right val -> Right val

runUrl :: String -> Either String Url
runUrl input = runUrl' parseUrl input


{- broken
--parseArgvWord = manyTill (anyChar) (try $ string ":::")
parseArgvWord = many letter

parseArgv = sepBy parseArgvWord (string ":::")

runArgv' :: Parser [String] -> String -> Either String [String]
runArgv' p input = do
 case (parse p "Argv" input) of
  Left err -> Left $ "Parse error: " ++ show err
  Right val -> Right val

runArgv :: String -> Either String [String]
runArgv input = runArgv' parseArgv input
-}
