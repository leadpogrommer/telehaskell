{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

import Network.HTTP
import Network.HTTP.Server
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Server.Logger (stdLogger)
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Aeson ((.:))
import qualified Data.ByteString.Lazy.Char8 as Char8
import GHC.Generics
import Control.Applicative ( Alternative(empty) )
import Telegram
import Text.Regex.PCRE.Heavy
import Data.Time
import Control.Monad ( void, forever )
import System.Environment (getEnv, lookupEnv)

import Db
import Database.MongoDB (Pipe)
import Control.Concurrent
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe (isJust)




fromMaybeToIO:: Maybe a -> IO a
fromMaybeToIO (Just x) = return x
fromMaybeToIO Nothing = fail "Invalid reuest"


data TRequest = TRequest {chatID :: Integer, chatMessage:: String } deriving (Show, Generic)
instance A.FromJSON  TRequest where
    parseJSON (A.Object v) =  do
        msg <- v .: "message"
        text <- msg .: "text"
        chat <- msg .: "chat"
        id <- chat .: "id"
        return  (TRequest {chatID = id, chatMessage = text})
    parseJSON _ = empty


inDocker = do
    ev <- lookupEnv "IN_DOCKER"
    return $ isJust ev

nameOrLocalhost :: String -> IO String 
nameOrLocalhost s = do
    d <- inDocker
    return $ if d then s else "127.0.0.1" 


getTg = do
    token <- getEnv "BOT_TOKEN"
    apiIp <- nameOrLocalhost "telegram-bot-api"
    return $ TConnectionData ("http://" ++ apiIp ++ ":8081") token




main :: IO ()
main = do
    dbIp <- nameOrLocalhost "db"
    myIp <- nameOrLocalhost "bot"
    tg <- getTg
    print tg
    print dbIp
    print myIp
    db <- getConnection dbIp
    forkIO $ notificationLoop db
    result <- setWebHook tg ("http://" ++ myIp ++ ":8080")
    body <- getResponseBody result
    print body
    serverWith (Config stdLogger "0.0.0.0" 8080) (requestHndler db)
    

-- connection - telegram, conn - mongodb
requestHndler:: Pipe -> Handler B.ByteString  
requestHndler db addr url request = do
    tg <- getTg
    let rq =  rqBody request
    val <- fromMaybeToIO (A.decode rq :: Maybe TRequest)
    let command = parseRequest $ chatMessage val
    putStrLn $ "Processing command: " ++ (show command)
    processCommand tg db (chatID val) command 
    return $ respond OK
 

data Command = ErrorCommand String | StartCommand | HelpCommand | RemindCommand UTCTime String deriving Show 

processCommand tg db cID command = pc command
    where msg t = void $ sendMessage tg cID t
          pc:: Command -> IO ()
          pc (ErrorCommand s) = msg $ "Error: " ++ s 
          pc StartCommand = msg "Welcome!" -- TODO: register user in db
          pc HelpCommand = msg "Really helpful text goes here..."
          pc (RemindCommand time d) = do
                msg "Ok"
                createNotification db cID time d
                return ()
              



parseRequest:: String -> Command
parseRequest string | scanRes == [] = ErrorCommand "You need to specify a command starting with \"/\""
                    | otherwise = parseCommand $ head $ snd $ head scanRes
    where scanRes = scan [re|^\/(\w+).*|] string
          parseCommand "start" = StartCommand
          parseCommand "help" = HelpCommand
          parseCommand "remind" | remindRes == [] = ErrorCommand "Use format /remind YYYY-MM-DD HH:MM your message"
                                | otherwise  = remindCommand time
                            where remindRes = scan [re|^\/remind (\d\d\d\d-\d\d-\d\d \d\d:\d\d) (.*)|] string
                                  time = parseTimeM True defaultTimeLocale "%Y-%-m-%-d %H:%M" ((snd $ head remindRes) !! 0) :: Maybe UTCTime 
                                  remindCommand Nothing  = ErrorCommand "Invlid time"
                                  remindCommand (Just t) = RemindCommand t ((snd $ head remindRes) !! 1)
          parseCommand _ = ErrorCommand "Unknown command"
                                  


notificationLoop db = forever (do 
    tg <- getTg
    time <- getPOSIXTime
    -- print time
    toSend <- getCurrentNotificationsAndDeleteThem db (round time + (7*60*60))
    mapM_ (\(c, t) -> sendMessage tg c t) toSend
    threadDelay 1000000          
    )