{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Telegram (TConnectionData(TConnectionData), setWebHook, sendMessage) where
import Network.HTTP
import qualified Data.Aeson as A
import qualified Network.Stream as NS
import qualified Data.ByteString.Lazy.Char8 as Char8
import GHC.Generics

data TConnectionData = TConnectionData {url:: String, token:: String} deriving (Show)

tRequest :: A.ToJSON  a => TConnectionData -> String -> a -> IO (NS.Result (Response String))
tRequest (TConnectionData u t) endpoint a = do
    let body = A.encode a
    simpleHTTP (postRequestWithBody (u ++ "/bot" ++ t ++ "/" ++ endpoint) "application/json" (Char8.unpack body))
    

data SetWebHookRequest = SetWebHookRequest {url::String, allowed_updates::String, max_connections::Integer, drop_pending_updates:: Bool } deriving (Generic, Show)
instance A.ToJSON SetWebHookRequest
setWebHook conn url = tRequest conn "setWebhook" (SetWebHookRequest url "message" 1 True )
setWebHook :: TConnectionData -> String -> IO (NS.Result (Response String))


data SendMessageRequest = SendMessageRequest {chat_id:: Integer, text::String}  deriving (Generic, Show)
instance A.ToJSON SendMessageRequest
sendMessage ::  TConnectionData -> Integer -> String -> IO (NS.Result (Response String))
sendMessage conn chatID text = tRequest conn "sendMessage" (SendMessageRequest chatID text)
