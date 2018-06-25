{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified Network.WebSockets as WS
import Network.WebSockets 
import Pipes
import qualified Pipes.Prelude as P
import Pipes.WebSockets
import Pipes.Concurrent
import Data.Text
import Control.Monad.Trans.Reader (ReaderT (runReaderT))

main :: IO ()
main = do
  putStrLn "hello world"

  -- runClientWith host port path opts headers clientApp



run :: String  -- ^ host
    -> Int -- ^ port
    -> String -- ^ path
    -> Headers
    -> Connection -> IO ()
run host port path headers c = do

    -- Setup a "mailbox" for getting messages from and placing them in.
    (output, input) <- spawn unbounded
    
                 -- Take messages from the socket and put them in the output.
    let msgsFrom = runEffect $ wsIn >-> toOutput output
                 -- Read from stdin and send msgs to the server.
        msgsTo   = runEffect $ for P.stdinLn
                                   (\s -> yield (pack s) >-> wsOut)
        --
        --  Read messages from the socket (which have been placed on the
        --  mailbox) and print to stdout.
        msgsOut  = runEffect $ for (fromInput input) 
                                   (\s -> lift $ putStrLn ("Received: " ++ unpack s))

    lvf $ msgsOut

    let clientApp = \c -> do
                                lvf $ runReaderT msgsFrom c
                                runReaderT msgsTo c

    WS.runClientWith host port path defaultConnectionOptions headers clientApp
  where
    lvf = liftIO . void . forkIO
