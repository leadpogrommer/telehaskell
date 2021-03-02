{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Db(createNotification, getConnection, getCurrentNotificationsAndDeleteThem) where

import Database.MongoDB
import Data.Time
import Data.Time.Clock.POSIX


getConnection :: String  -> IO Pipe
getConnection ip = connect $ host ip

run ::  Pipe -> Action IO a -> IO a
run conn = access conn master "notificator"



-- createNotification :: Pipe -> IO Value
createNotification :: Pipe -> Integer -> UTCTime  -> String -> IO Value
createNotification conn chatID time msg = run conn (do
    insert "notifications" ["chat" =: chatID, "text" =: msg, "time" =: (round $utcTimeToPOSIXSeconds time)]
    )

-- getCurrentNotificationsAndDeleteThem::  Pipe  -> Integer


-- getCurrentNotificationsAndDeleteThem :: Pipe -> Integer  -> IO [Document]
getCurrentNotificationsAndDeleteThem :: Pipe -> Integer -> IO [(Integer, String)]
getCurrentNotificationsAndDeleteThem conn timestamp = do
    conns <- run conn (do
                cursor <- find (select ["time" =: ["$lt" =: timestamp]] "notifications")
                rest cursor
            )
    run conn (do
        delete $ select ["time" =: ["$lt" =: timestamp]] "notifications"
        )
    return [("chat" `at` d, "text" `at` d) | d <- conns]



