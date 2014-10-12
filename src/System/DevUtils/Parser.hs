module System.DevUtils.Parser (
 module A,
 parseCmd,
 runCmd
) where

import Text.Parsec
import Text.Parsec.String

import System.DevUtils.Parser.Cmd as A

import qualified System.DevUtils.Base.Url.Redis as R
import qualified System.DevUtils.Base.Url.Memcache as Memcache
import qualified System.DevUtils.Base.Url.MySQL as M
import qualified System.DevUtils.Base.Url.Ssh as S
import qualified System.DevUtils.Base.Url.ZMQ as ZMQ
import qualified System.DevUtils.Base.Url.File as F
import qualified System.DevUtils.Base.Url.Auth as A
import qualified System.DevUtils.Base.Url.Connection as C
import qualified System.DevUtils.Base.Url.Session as Ses

import System.DevUtils.Base.Url.Field


import Data.Word
 (Word16)


type St a = GenParser Char Cmd a


password = between (char '(') (char ')') (many1 $ noneOf "()")
field = many1 $ noneOf ": "
field' s = many1 $ noneOf s
port = do
 s <- try (many1 digit) <?> "digits"
 let i = read s :: Integer
 if (0 >= i || i > 65535) then return "port too large" else return s

redisKey = many1 $ noneOf ",/ "
mysqlDb = many1 $ (letter <|> oneOf "_-:")


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
parseUrlConnectionWrapper :: C.Connection -> C.ConnectionType -> St Cmd
parseUrlConnectionWrapper defCon conType = do
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
 do { (try (string "con://")) ; parseUrlConnectionWrapper defCon C.UNKNOWN }
 <|> do { (try (string "tcp://")) ; parseUrlConnectionWrapper defCon C.TCP }
 <|> do { (try (string "udp://")) ; parseUrlConnectionWrapper defCon C.UDP }
 <|> do { (try (string "unix://")) ; parseUrlConnectionWrapper defCon C.UNIX }
 <?> "prefix"


{-
 - file
 - host:port
 -}

parseUrlConnection :: C.Connection -> St Cmd
parseUrlConnection defCon = do
 dest <- field
 let portNum = C._port defCon
 maybePort <- try $ optionMaybe $ (try $ string ":" >> port)
 (putState $ UrlConnection C.Connection { C._dest = dest , C._port = (maybe portNum (\x -> read x :: Word16) maybePort), C._type = C.UNKNOWN }) >> getState >>= return


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
 (Just (UrlConnection con)) <- optionMaybe (
  (try $ parseUrlConnection'' (Ses._con defSes))
  <|> (try $ parseUrlConnection (Ses._con defSes))
  <?> "connection")
 (putState $ UrlSession st { Ses._con = con }) >> getState >>= return


{-
 - resque:// same as redis://
 -}

parseUrlResque' :: St Cmd
parseUrlResque' = do
 _ <- string "resque://"
 (UrlRedis red) <- parseUrlRedis
 return $ UrlResque red

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
 (try $ parseUrlSession R.defaultRedisSession) >>= \(UrlSession ses) ->
  (putState $ UrlRedis R.defaultRedis { R._ses = ses }) >>
   getState >>= \(UrlRedis st) ->
    option (UrlRedis st) (try (char '/' >> parseUrlRedisOptions) >>= \(UrlRedis stopts) ->
     option (UrlRedis stopts) (try (char '/' >> many1 anyToken) >>= \(s) ->
      return (UrlRedis stopts { R._custom = (Just s) })))
 <?> "redis"

parseUrlRedisOptionsDb :: St Cmd
parseUrlRedisOptionsDb = do
 _ <- string "db="
 (UrlRedis s) <- getState
 num <- many1 digit
 return $ UrlRedis s { R._db = (read num :: Integer) }

parseUrlRedisOptionsPool :: St Cmd
parseUrlRedisOptionsPool = do
 _ <- string "pool="
 (UrlRedis s) <- getState
 num <- many1 digit
 return $ UrlRedis s { R._pool = (read num :: Integer) }

parseUrlRedisOptionsIdle :: St Cmd
parseUrlRedisOptionsIdle = do
 _ <- string "idle="
 (UrlRedis s) <- getState
 num <- many1 digit
 return $ UrlRedis s { R._idle = (read num :: Integer) }

parseUrlRedisOptionsPrefix :: St Cmd
parseUrlRedisOptionsPrefix = do
 _ <- string "prefix="
 (UrlRedis s) <- getState
 rkey <- redisKey
 return $ UrlRedis s { R._prefix = Just rkey }

parseUrlRedisOptionsKey :: St Cmd
parseUrlRedisOptionsKey = do
 _ <- string "key="
 (UrlRedis s) <- getState
 rkey <- redisKey
 return $ UrlRedis s { R._key = Just rkey }

parseUrlRedisOptions :: St Cmd
parseUrlRedisOptions = do
 (UrlRedis st) <- (try parseUrlRedisOptionsDb
        <|> try parseUrlRedisOptionsPool
        <|> try parseUrlRedisOptionsIdle
        <|> try parseUrlRedisOptionsPrefix
        <|> try parseUrlRedisOptionsKey
        <?> "redisOption"
       )
 putState (UrlRedis st)
 v <- option (UrlRedis st) (try $ char ',' >> parseUrlRedisOptions)
 return v


{- TODO FIXME
 - memcache://
 - memcache://host
 - memcache://host:port
 - etc.
 -}

parseUrlMemcache' :: St Cmd
parseUrlMemcache' = do
 _ <- string "memcache://"
 parseUrlMemcache'

parseUrlMemcache :: St Cmd
parseUrlMemcache = do
 putState $ UrlMemcache Memcache.defaultMemcache
 (UrlMemcache memcache) <- getState
 (UrlConnection con) <- (
  (try $ parseUrlConnection Memcache.defaultMemcacheConnection)
  <|> (try $ parseUrlConnection'' Memcache.defaultMemcacheConnection)
  <?> "connection")
 (putState $ UrlMemcache memcache { Memcache._con = con }) >> getState >>= return

{-
 - mysql://user:(pass)@host
 - mysql://user:(pass)@host:port
 - mysql://user:(pass)@host/options
 - mysql://user:(pass)@host:port/options
 - etc.
 -}
parseUrlMySQL' :: St Cmd
parseUrlMySQL' = do
 _ <- string "mysql://"
 parseUrlMySQL

parseUrlMySQL :: St Cmd
parseUrlMySQL = do
 (try $ parseUrlSession M.defaultMySQLSession) >>= \(UrlSession ses) ->
  (putState $ UrlMySQL M.defaultMySQL { M._ses = ses }) >>
   getState >>= \(UrlMySQL st) ->
    option (UrlMySQL st) (try (char '/' >> parseUrlMySQLOptions) >>= \(UrlMySQL stopts) ->
     option (UrlMySQL stopts) (try (char '/' >> many1 anyToken) >>= \(s) ->
      return (UrlMySQL stopts { M._custom = (Just s) })))
 <?> "mysql"

parseUrlMySQLOptionsDb :: St Cmd
parseUrlMySQLOptionsDb = do
 _ <- string "db="
 (UrlMySQL s) <- getState
 name <- mysqlDb
 return $ UrlMySQL s { M._db = Just name }

parseUrlMySQLOptions :: St Cmd
parseUrlMySQLOptions = do
 (UrlMySQL st) <- (try parseUrlMySQLOptionsDb
        <?> "mysqlOption"
       )
 putState (UrlMySQL st)
 v <- option (UrlMySQL st) (try $ char ',' >> parseUrlMySQLOptions)
 return v

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
 (UrlSession ses) <- try $ parseUrlSession S.defaultSshSession
 (putState $ UrlSsh ssh { S._ses = ses }) >> getState >>= return

{-
 - zmq://host
 - zmq://host:port
 - zmq://host:port/options
 - zmq://host/options
 - zmq://user:(pass)@host
 - zmq://user:(pass)@host/options
 - etc.
 -}
parseUrlZMQ' :: St Cmd
parseUrlZMQ' = do
 _ <- string "zmq://"
 parseUrlZMQ

parseUrlZMQ :: St Cmd
parseUrlZMQ = do
 putState $ UrlZMQ ZMQ.defaultZMQ
 (UrlZMQ zmq) <- getState
 (UrlConnection con) <- (
  (try $ parseUrlConnection ZMQ.defaultZMQConnection)
  <|> (try $ parseUrlConnection'' ZMQ.defaultZMQConnection)
  <?> "connection")
 (putState $ UrlZMQ zmq { ZMQ._con = con }) >> getState >>= return

{-
 - file://path
 -}
parseUrlFile' :: St Cmd
parseUrlFile' = do
 _ <- string "file://"
 parseUrlFile

parseUrlFile :: St Cmd
parseUrlFile = do
 s <- many1 anyToken
 return $ UrlFile F.File { F._path = s }


{-
 - chan://
 -}

parseUrlChan' :: St Cmd
parseUrlChan' = do
 _ <- string "chan://"
 return UrlChan

{-
 - mvar://
 -}

parseUrlMVar' :: St Cmd
parseUrlMVar' = do
 _ <- string "mvar://"
 return UrlMVar

{-
 - ioref://
 -}

parseUrlIORef' :: St Cmd
parseUrlIORef' = do
 _ <- string "ioref://"
 return UrlIORef

-- URLS

parseUrl :: St Cmd
parseUrl = do
 (try parseUrlAuth')
 <|> (try parseUrlSession')
 <|> (try parseUrlRedis')
 <|> (try parseUrlMemcache')
 <|> (try parseUrlMySQL')
 <|> (try parseUrlResque')
 <|> (try parseUrlSsh')
 <|> (try parseUrlZMQ')
 <|> (try parseUrlFile')
 <|> (try parseUrlConnection')
 <|> (try parseUrlChan')
 <|> (try parseUrlMVar')
 <|> (try parseUrlIORef')
 <?> "url"

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
 _ <- string $ delim : "://"
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
 try parseUrl
 <|> (try parseSep)
 <?> "cmd"

runCmd' :: St Cmd -> String -> Either String Cmd
runCmd' p input = do
 case (runParser p CmdNone "Cmd" input) of
  Left err -> Left $ "Parse error: " ++ show err
  Right val -> Right val

runCmd :: String -> Either String Cmd
runCmd input = runCmd' parseCmd input
