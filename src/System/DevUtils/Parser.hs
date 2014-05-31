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
import qualified System.DevUtils.Session as Ses
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
parseUrlConnectionWrapper :: C.Connection -> String -> C.ConnectionType -> St Url
parseUrlConnectionWrapper defCon prefix conType = do
 _ <- parseUrlConnection defCon
 modifyState (\(UrlConnection x) -> UrlConnection x { C._type = conType })
 getState >>= return

{-
 - con://unix
 - con://host:port
 -}

parseUrlConnection' :: St Url
parseUrlConnection' = parseUrlConnection'' C.defaultConnection

parseUrlConnection'' :: C.Connection -> St Url
parseUrlConnection'' defCon = do
 do { (try (string "con://")) ; parseUrlConnectionWrapper defCon "con" C.UNKNOWN }
 <|> do { (try (string "tcp://")) ; parseUrlConnectionWrapper defCon "tcp" C.TCP }
 <|> do { (try (string "udp://")) ; parseUrlConnectionWrapper defCon "udp" C.UDP }
 <|> do { (try (string "unix://")) ; parseUrlConnectionWrapper defCon "unix" C.UNIX }
 <?> "prefix"


{-
 - file
 - host:port
 -}

parseUrlConnection :: C.Connection -> St Url
parseUrlConnection defCon = do
 dest <- field
 let portNum = C._port defCon
 maybePort <- try $ optionMaybe $ (string ":" >> port)
 (putState $ UrlConnection C.Connection { C._dest = dest , C._port = (maybe portNum (\x -> read x :: Integer) maybePort), C._type = C.UNKNOWN }) >> getState >>= return


{-
 - session
 -}
parseUrlSession' :: St Url
parseUrlSession'= do
 parseUrlSession'' "session"

parseUrlSession'' :: String -> St Url
parseUrlSession'' arg = do
 _ <- string $ arg ++ "://"
 parseUrlSession Ses.defaultSession

parseUrlSession :: Ses.Session -> St Url
parseUrlSession defSes = do
 putState $ UrlSession defSes
 auth <- optionMaybe (try parseUrlAuth)
 _ <- case auth of
  Just (UrlAuth val) -> do
   _ <- char '@'
   (putState $ UrlSession Ses.Session { Ses._auth = Just val }) >> getState >>= return
  _ -> do
   return UrlNone
 (UrlSession st) <- getState
 (Just (UrlConnection con)) <- optionMaybe (parseUrlConnection'' (Ses._con defSes))
 (putState $ UrlSession st { Ses._con = con }) >> getState >>= return


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
 (UrlSession ses) <- parseUrlSession R.defaultRedisSession <?> "session"
 (putState $ UrlRedis R.Redis { R._ses = ses }) >> getState >>= return


{-
 - ssh://host
 - ssh://host:port
 - ssh://host:port/options
 - ssh://host/options
 - ssh://user:(pass)@host
 - ssh://user:(pass)@host/options
 - etc.
 -}
parseUrlSsh' :: St Url
parseUrlSsh' = do
 _ <- string "ssh://"
 parseUrlSsh

parseUrlSsh :: St Url
parseUrlSsh = do
 putState $ UrlSsh S.defaultSsh
 (UrlSsh ssh) <- getState
 (UrlSession ses) <- parseUrlSession S.defaultSshSession <?> "session"
 (putState $ UrlSsh ssh { S._ses = ses }) >> getState >>= return


{- URL PARSING -}

parseUrl :: St Url
parseUrl = do
 (try parseUrlAuth')
 <|> (try parseUrlSession')
 <|> (try parseUrlRedis')
 <|> (try parseUrlSsh')
 <|> (try parseUrlConnection')
 <?> "url"

runUrl' :: St Url -> String -> Either String Url
runUrl' p input = do
 case (runParser p UrlNone "Url" input) of
  Left err -> Left $ "Parse error: " ++ show err
  Right val -> Right val

runUrl :: String -> Either String Url
runUrl input = runUrl' parseUrl input
