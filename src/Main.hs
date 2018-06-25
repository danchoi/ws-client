{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified Network.WebSockets as WS
import Data.Char (isAlpha)
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
import Wuss

data Protocol = Ws | Wss
    deriving Show

data Options = Options {
    _protocol :: Protocol
  , _host :: String
  , _port :: Int
  , _path :: String
  , _sessid :: B8.ByteString
  } deriving Show

options :: Parser Options
options = Options 
  <$> argument readProtocol (metavar "protocol")
  <*> strArgument (metavar "HOST")
  <*> argument auto (metavar "PORT")
  <*> strArgument (metavar "PATH")
  <*> (B8.pack <$> strArgument (metavar "_sessid"))


readProtocol :: ReadM Protocol
readProtocol = eitherReader $ \s -> 
    case Prelude.takeWhile (isAlpha) s of
      "ws" -> Right Ws
      "wss" -> Right Wss
      x -> Left $ "Invalid protocol: " ++ x

opts :: ParserInfo Options
opts = info (options <**> helper)
            (fullDesc <> header "ws-client")

main :: IO ()
main = do
  Options{..} <- execParser opts
  let hs = [("Cookie", "_sessid="<> _sessid)]
  run _protocol _host _port _path [] -- hs

run :: Protocol
    -> String  -- ^ host
    -> Int -- ^ port
    -> String -- ^ path
    -> Headers
    -> IO ()
run proto host port path headers = do

    -- Setup a "mailbox" for getting messages from and placing them in.
    (output, input) <- spawn unbounded
    
    let 

        msgsFrom  = runEffect $ wsIn 
                        >-> P.chain (\s -> lift $ putStrLn $ "Received 1: " ++ unpack s)
                        >-> toOutput output

    lvf $ runEffect $ for (fromInput input)  
                          (\s -> lift $ putStrLn ("Received: " ++ unpack s))

    let clientApp = \c -> lvf $ runReaderT msgsFrom c


    case proto of
      Wss -> Wuss.runSecureClientWith host (fromIntegral port) path defaultConnectionOptions headers clientApp
      Ws -> WS.runClientWith host port path defaultConnectionOptions headers clientApp
  where
    lvf = liftIO . void . forkIO
