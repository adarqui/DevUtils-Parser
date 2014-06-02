module System.DevUtils.Parser (
 parseCmd,
 runCmd,
) where

import Text.Parsec
import Text.Parsec.String

import qualified Control.Applicative as APP

import qualified System.DevUtils.Redis as R
import qualified System.DevUtils.Ssh as S
import qualified System.DevUtils.Auth as A
import qualified System.DevUtils.Connection as C
import qualified System.DevUtils.Session as Ses

import System.DevUtils.Cmd
import System.DevUtils.Field
--import System.DevUtils.Url
--import System.DevUtils.Argv


type St a = GenParser Char Cmd a


password = between (char '(') (char ')') (many1 $ noneOf "()")
field = many1 $ noneOf ": "
field' s = many1 $ noneOf s
port = do
 s <- try (many1 digit) <?> "digits"
 let i = read s :: Integer
 if (0 >= i || i > 65535) then return "port too large" else return s


{-
 - auth://user
 - auth://user:(pass)
 -}

parseUrlAuth' :: St Cmd
parseUrlAuth' = do
 _ <- string "auth://"
 parseUrlAuth


{-
 - user
 - user:(pass)
 -}

parseUrlAuth :: St Cmd
parseUrlAuth = do
 user <- field
 pass <- try $ optionMaybe $ (string ":" >> password)
 (putState $ UrlAuth A.Auth { A._user = user, A._pass = pass }) >> getState >>= return

-- con:// wrapper
parseUrlConnectionWrapper :: C.Connection -> String -> C.ConnectionType -> St Cmd
parseUrlConnectionWrapper defCon prefix conType = do
 _ <- parseUrlConnection defCon
 modifyState (\(UrlConnection x) -> UrlConnection x { C._type = conType })
 getState >>= return

{-
 - con://unix
 - con://host:port
 -}

parseUrlConnection' :: St Cmd
parseUrlConnection' = parseUrlConnection'' C.defaultConnection

parseUrlConnection'' :: C.Connection -> St Cmd
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

parseUrlConnection :: C.Connection -> St Cmd
parseUrlConnection defCon = do
 dest <- field
 let portNum = C._port defCon
 maybePort <- try $ optionMaybe $ (string ":" >> port)
 (putState $ UrlConnection C.Connection { C._dest = dest , C._port = (maybe portNum (\x -> read x :: Integer) maybePort), C._type = C.UNKNOWN }) >> getState >>= return


{-
 - session
 -}
parseUrlSession' :: St Cmd
parseUrlSession'= do
 parseUrlSession'' "session"

parseUrlSession'' :: String -> St Cmd
parseUrlSession'' arg = do
 _ <- string $ arg ++ "://"
 parseUrlSession Ses.defaultSession

parseUrlSession :: Ses.Session -> St Cmd
parseUrlSession defSes = do
 putState $ UrlSession defSes
 auth <- optionMaybe (try parseUrlAuth)
 _ <- case auth of
  Just (UrlAuth val) -> do
   _ <- char '@'
   (putState $ UrlSession Ses.Session { Ses._auth = Just val }) >> getState >>= return
  _ -> do
   return CmdNone
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
parseUrlRedis' :: St Cmd
parseUrlRedis' = do
 _ <- string "redis://"
 parseUrlRedis

parseUrlRedis :: St Cmd
parseUrlRedis = do
 (UrlSession ses) <- parseUrlSession R.defaultRedisSession <?> "session"
 (putState $ UrlRedis R.defaultRedis { R._ses = ses }) >> getState >>= return
 {-
 (putState $ UrlRedis R.defaultRedis { R._ses = ses })
 (UrlRedis st) <- try parseUrlRedisOptions
 (putState $ UrlRedis st)
 getState >>= return
 -}
-- // FIXME, options parser

parseUrlRedisOptionsPool :: St Cmd
parseUrlRedisOptionsPool = do
 string "pool="
 (UrlRedis s) <- getState
 return $ UrlRedis s { R._pool = 5 }

parseUrlRedisOptionsIdle :: St Cmd
parseUrlRedisOptionsIdle = do
 string "idle="
 (UrlRedis s) <- getState
 return $ UrlRedis s { R._idle = 5 }

parseUrlRedisOptions :: St Cmd
parseUrlRedisOptions = do
-- f <- sepBy (field' (", ")) (skipMany1 (space <|> char ','))
 char '/'
 try parseUrlRedisOptionsPool
 <|> try parseUrlRedisOptionsIdle
 <?> "redisOption"

{-
 - ssh://host
 - ssh://host:port
 - ssh://host:port/options
 - ssh://host/options
 - ssh://user:(pass)@host
 - ssh://user:(pass)@host/options
 - etc.
 -}
parseUrlSsh' :: St Cmd
parseUrlSsh' = do
 _ <- string "ssh://"
 parseUrlSsh

parseUrlSsh :: St Cmd
parseUrlSsh = do
 putState $ UrlSsh S.defaultSsh
 (UrlSsh ssh) <- getState
 (UrlSession ses) <- parseUrlSession S.defaultSshSession <?> "session"
 (putState $ UrlSsh ssh { S._ses = ses }) >> getState >>= return


-- seps

parseSepComma :: St Cmd
parseSepComma = parseSep' ','

parseSepColon :: St Cmd
parseSepColon = parseSep' ':'

parseSepSemi :: St Cmd
parseSepSemi = parseSep' ';'

parseSepDot :: St Cmd
parseSepDot = parseSep' '.'

parseSep' :: Char -> St Cmd
parseSep' delim = do
 string $ delim : "://"
 parseSep'' delim

parseSep'' :: Char -> St Cmd
parseSep'' delim = do
 f <- sepBy (field' (delim : " ")) (skipMany1 (space <|> char delim))
 return $ SepFields Fields { _delim = [delim], _memb = f }

parseSep :: St Cmd
parseSep = do
 try parseSepComma
 <|> try parseSepColon
 <|> try parseSepSemi
 <|> try parseSepDot
 <?> "sep"

{- CMD PARSING -}

parseCmd :: St Cmd
parseCmd = do
 (try parseUrlAuth')
 <|> (try parseUrlSession')
 <|> (try parseUrlRedis')
 <|> (try parseUrlSsh')
 <|> (try parseUrlConnection')
 <|> (try parseSep)
 <?> "cmd"

runCmd' :: St Cmd -> String -> Either String Cmd
runCmd' p input = do
 case (runParser p CmdNone "Cmd" input) of
  Left err -> Left $ "Parse error: " ++ show err
  Right val -> Right val

runCmd :: String -> Either String Cmd
runCmd input = runCmd' parseCmd input
