module Main where

import qualified System.IO                     as SIO
import           System.IO
import           Control.Exception
import           Api.Client
import qualified Yesod.Auth.Util.PasswordStore as PS
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.UTF8     as BLU
import qualified Data.ByteString.Base64.Lazy   as B64
import qualified Encryption.Globals            as EG
import           Control.Monad                  ( void )

main :: IO ()
main = void repl

data State = State
  { getStateToken    :: Maybe Token
  , getStateVaultKey :: Maybe EG.Key
  , getStateData     :: Maybe [Entry]
  } -- state of the application

emptyState :: State
emptyState = State Nothing Nothing Nothing

welcomeMessage :: String
welcomeMessage =
  "Welcome to the McFerrin password manager! This is a CLI tool that lets you communicate with the server."

repl :: IO State
repl = putStrLn welcomeMessage >> loop emptyState
 where
  loop :: State -> IO State
  loop state = do
    let secrets = do
          token    <- getStateToken state
          vaultKey <- getStateVaultKey state
          return (token, vaultKey)
    case secrets of
      Nothing -> do
        prompt "use commands 'login' or 'register'"
        command            <- getLine
        (username, master) <- inputCredentials
        let vaultKey  = genVaultKey master username
        let loginHash = genLoginHash vaultKey username
        case command of
          "login" -> do
            result <- login username loginHash
            case result of
              Left  err   -> putStrLn err >> loop state
              Right token -> putStrLn "login successful!"
                >> loop (State (Just token) (Just vaultKey) Nothing)
          "register" -> do
            result <- register username loginHash
            case result of
              Left  err  -> putStrLn err >> loop state
              Right user -> do
                putStrLn
                  $  "New account created. here's some info about it:\n"
                  <> show user
                  <> "To get started, log into your account."
                loop state
          "q" -> quit state
          _   -> loop state
      Just (token, vaultKey) -> do
        prompt
          "available commands: 'deleteUser', 'getData', 'addEntries', 'removeEntries'"
        command <- getLine
        case command of
          "deleteUser" -> do
            result <- deleteUser token
            case result of
              Left  err -> putStrLn err >> loop state
              Right _   -> putStrLn "Account deleted." >> loop emptyState
          "getData" -> do
            result <- getData token vaultKey
            case result of
              Left  err     -> putStrLn err >> loop state
              Right entries -> print entries >> loop state
          "addEntries"    -> loop state
          "removeEntries" -> loop state
          "logOut"        -> loop emptyState
          "q"             -> quit state
          _               -> loop state

quit :: State -> IO State
quit state = putStrLn "quitting" >> pure state

prompt :: String -> IO ()
prompt msg = putStr (msg <> "\n>>>")

inputNewEntry :: IO Entry
inputNewEntry =
  Entry
    <$> (prompt "Input url:" >> withEcho True getLine)
    <*> (prompt "Input username:" >> withEcho True getLine)
    <*> (prompt "Input password (hidden):" >> getPassword)

inputCredentials :: IO (String, String)
inputCredentials = do
  prompt "Input your mcferrin username:"
  username <- withEcho True getLine
  prompt "Input your mcferrin password (input hidden):"
  master <- getPassword
  return (username, master)


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
genVaultKey master username = B.fromStrict $ PS.pbkdf2
  (B.toStrict $ BLU.fromString master)
  (PS.makeSalt (B.toStrict $ zfill 8 (BLU.fromString username)))
  100100

genLoginHash' :: String -> String -> String
genLoginHash' master username =
  BLU.toString $ B64.encode $ B.fromStrict $ PS.pbkdf2
    (B.toStrict $ BLU.fromString master)
    (PS.makeSalt (B.toStrict $ zfill 8 (BLU.fromString username)))
    100101

genLoginHash :: B.ByteString -> String -> String
genLoginHash vaultKey username =
  BLU.toString $ B64.encode $ B.fromStrict $ PS.pbkdf2
    (B.toStrict vaultKey)
    (PS.makeSalt (B.toStrict $ zfill 8 (BLU.fromString username)))
    1


zfill :: Int -> B.ByteString -> B.ByteString
zfill n bs
  | B.length bs >= fromIntegral n = bs
  | otherwise = B.take (fromIntegral n) $ bs `B.append` B.replicate
    (fromIntegral n)
    0

-- PS.pbkdf2 (BSU.fromString "HashMe") (PS.makeSalt (BSU.fromString "SALT8888")) 100000
