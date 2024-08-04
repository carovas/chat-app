module Main where

import System.IO
import Control.Concurrent

{-
asks for an ip adress (the port will always be 6969)
connect to the server

two threads:
- receiving thread get messages from the server
- sending thread read input from user and sends it
-}

{-
sending thread
get message from user
send it
call the function again
-}
sendLoop :: Chan () -> IO ()
sendLoop endFlags = do
  x <- getLine
  if x == "exit"
  then writeChan endFlags ()
  else do
    putStrLn x
    sendLoop endFlags

main :: IO ()
main = do 
  endFlags <- newChan
  forkIO $ sendLoop endFlags

  --mapM_ (\_ -> readChan endFlags) [1..1]
