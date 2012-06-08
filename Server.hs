import Control.Monad
import Control.Concurrent
import System.IO
import Control.Exception
import Data.List
import Network

main = do
  -- input channel for events
  events <- newChan
  -- client channels
  outs <- newMVar []
  -- thread that moves data from input channel to output channels
  forkIO $ requeue events outs
  -- listen for clients on port 8960 and fork for each one
  -- some clients will be inputs, some will be outputs
  listen (PortNumber 8960) (client events outs) (source events)

listen port client source = do
  socket <- listenOn port
  forever $ do
    (handle, host, port) <- accept socket
    hSetBuffering handle LineBuffering
    forkIO $ do
      line <- hGetLine handle
      when ("source" `isPrefixOf` line) $ source handle >> return ()
      when ("client" `isPrefixOf` line) $ client handle >> return ()
      putStrLn $ "got type: " ++ show line

requeue events outs = forever $ do
  e <- readChan events
  queues <- readMVar outs
  forM_ queues $ \q -> do
    writeChan q e

-- listen for things and put them in the events chan
source events handle = forkIO $ do
  putStrLn "got source"
  (forever $ hGetLine handle >>= writeChan events) `finally` return ()
  putStrLn "lost source"
  hClose handle
  return ()

-- wait for events, then send them over the handle
client events outs handle = forkIO $ do
  out <- newChan
  modify outs (out:)
  putStrLn "got client"
  -- get stuff from chan and send to handle
  (forever $ readChan out >>= hPutStrLn handle) `finally` modify outs (delete out)
  putStrLn "lost client"
  hClose handle
  return ()

modify mvar f = modifyMVar_ mvar (return . f)

