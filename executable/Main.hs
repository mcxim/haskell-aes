module Main where

import qualified System.IO                     as SIO
import           System.IO
import           Control.Exception
import           Api.Client
import qualified Yesod.Auth.Util.PasswordStore as PS
import qualified Data.ByteString               as B
import qualified Data.ByteString.UTF8          as BSU

main :: IO ()
main = getPassword >>= putStrLn . ("Entered: " ++)

repl :: IO ()
repl = do
  putStr "\n>>>"
  input <- withEcho True getLine
  case input of
    _ -> undefined
    _ -> undefined

prompt :: IO ()
prompt = putStr "\n>>>"

inputNewEntry :: IO Entry
inputNewEntry =
  Entry
    <$> (putStr "Input url:\n>>>" >> withEcho True getLine)
    <*> (putStr "Input username:\n>>>" >> withEcho True getLine)
    <*> (putStr "Input password (hidden):\n>>>" >> getPassword)


getPassword :: IO String
getPassword = do
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

genVaultKey :: String -> String -> B.ByteString
genVaultKey master username =
  PS.pbkdf2 (BSU.fromString master) (PS.makeSalt (zfill 8 (BSU.fromString username))) 100100

genLoginHash' :: String -> String -> B.ByteString
genLoginHash' master username =
  PS.pbkdf2 (BSU.fromString master) (PS.makeSalt (zfill 8 (BSU.fromString username))) 100101

genLoginHash :: B.ByteString -> String -> B.ByteString
genLoginHash vaultKey username =
  PS.pbkdf2 vaultKey (PS.makeSalt (zfill 8 (BSU.fromString username))) 1


zfill :: Int -> B.ByteString -> B.ByteString
zfill n bs | B.length bs >= n = bs
           | otherwise        = B.take n $ bs `B.append` B.replicate n 0
  where len = B.length bs

-- PS.pbkdf2 (BSU.fromString "HashMe") (PS.makeSalt (BSU.fromString "SALT8888")) 100000
