module Main where

import Network
import Network.IRC.Bot
import Control.Concurrent
import Control.Monad
import qualified Data.Set as Set

import Control.Monad.IO.Class (liftIO)

import Network.IRC.Bot.Part.Channels
import Network.IRC.Bot.Part.Dice
import Network.IRC.Bot.Part.NickUser
import Network.IRC.Bot.Part.Ping

import System.Process
import System.Directory
import System.FilePath

conf = nullBotConf { 
         host = "chat.freenode.net"
       , logger = stdoutLogger Debug
       , port = PortNumber 6667
       , nick = "PicoPico"
       , user = nullUser { username = "PicoPico", hostname = "raptor", servername = "raptor", realname = "who knows" }
       , commandPrefix = "!"
       , channels = Set.fromList ["#picobot"]
}

debugPart :: BotMonad m => m ()
debugPart = do
  env <- askBotEnv
  m <- askMessage
  who <- whoami
  priv <- privMsg 

  liftIO (print ("###",m,who))
  liftIO (print ("$$$",priv))
  liftIO (print ("&&&",msg priv))

-- Example ~/.picobot file:
-- [("picobot","ab:cd:ef:01:23:45")]
--
  
readMacs :: IO [(String,String)]
readMacs = do
  conf <- getAppUserDataDirectory "picobot"
  b <- doesFileExist conf
  if not b then do
      print $ "Configuration file missing: " ++ conf
      return []
   else 
      read `fmap` readFile conf

wake mac = rawSystem "wol" [mac]

wakePart :: BotMonad m => m ()
wakePart = do
  priv <- privMsg
  macs <- liftIO readMacs

  let target = (head (receivers priv))
      send msg = sendCommand (PrivMsg Nothing [target] msg)

  case msg priv of
    "!help" -> send "Syntax: !wake"
    "!wake" -> do
             send "Waking up..."
             mapM_ (\ (host,mac) -> do
                      send ("> " ++ host)
                      -- exitCode <- liftIO $ wake mac
                      -- send (show exitCode)
                   ) macs
    ('!':_) -> send "Unknown command. See !help."
    _ -> return ()

main = do
  (tvChans,chPart) <- initChannelsPart (channels conf)
  threads <- simpleBot conf [pingPart, nickUserPart, chPart, wakePart]
  
  forever (threadDelay 1000000)
