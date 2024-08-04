{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

{-
asks for an ip adress (the port will always be 6969)
connect to the server

two threads:
- receiving thread get messages from the server
- sending thread read input from user and sends it
-}

-- get ip adress from the user
gets :: String -> IO(String)
gets msg = do
  putStrLn msg
  ip <- getLine
  return ip

{-
WORKING
sending thread
get message from user
send it
call the function again
-}

-- pack the string
packStr :: String -> C.ByteString
packStr s = C.pack s

sendLoop :: Chan () -> Socket -> String -> IO ()
sendLoop endFlags s name = do
  x <- getLine
  if x == "exit"
  then writeChan endFlags ()
  else do
    let msg = packStr (name++": "++x)
    if (length (name++"xx"++x)) > 1024
    then do
      putStr "<ERROR> message is too long"
      sendLoop endFlags s name
    else do
      sendAll s msg
      sendLoop endFlags s name

{-
receiving thread
call recv function
call putStrln
calls itself 
No channel because it needs to stop when user type exit (defined in the upper fonction)
-}

receiveLoop :: Socket -> IO () 
receiveLoop s = do
  msg <- recv s 1024 -- max size is 1024
  if S.null msg == False
  then do
    putStr "Received from "
    C.putStrLn msg
    receiveLoop s
  else
    receiveLoop s

{-
Run tcp client function from the example of the library
-}
-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

main :: IO ()
main = do
  ip <- gets "Put the adress of the server to connect to : "
  name <- gets "Put your username: "
  runTCPClient ip "6969" $ \s -> do
    endFlags <- newChan
    forkIO $ sendLoop endFlags s name
    forkIO $ receiveLoop s

    mapM_ (\_ -> readChan endFlags) [1..1]

{-
do 
  endFlags <- newChan
  --forkIO $ sendLoop endFlags
  sendLoop endFlags
-}

  --mapM_ (\_ -> readChan endFlags) [1..1]
