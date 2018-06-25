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
import Data.Monoid
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Options.Applicative
import qualified Data.ByteString.Char8 as B8


data Options = Options {
    _host :: String
  , _port :: Int
  , _path :: String
  , _sessid :: B8.ByteString
  } deriving Show

options :: Parser Options
options = Options 
  <$> strArgument (metavar "HOST")
  <*> argument auto (metavar "PORT")
  <*> strArgument (metavar "PATH")
  <*> (B8.pack <$> strArgument (metavar "_sessid"))

opts :: ParserInfo Options
opts = info (options <**> helper)
            (fullDesc <> header "ws-client")

main :: IO ()
main = do
  Options{..} <- execParser opts
  let hs = [("_sessid", _sessid)]
  run _host _port _path hs

run :: String  -- ^ host
    -> Int -- ^ port
    -> String -- ^ path
    -> Headers
    -> IO ()
run host port path headers = do

    -- Setup a "mailbox" for getting messages from and placing them in.
    (output, input) <- spawn unbounded
    
    let 

        msgsFrom  = runEffect $ wsIn >-> toOutput output

    lvf $ runEffect $ for (fromInput input)  
                          (\s -> lift $ putStrLn ("Received: " ++ unpack s))

    let clientApp = \c -> lvf $ runReaderT msgsFrom c


    WS.runClientWith host port path defaultConnectionOptions headers clientApp
  where
    lvf = liftIO . void . forkIO
