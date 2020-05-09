module Main where

import           System.IO
import           Control.Exception
import           Api.Client
import qualified Yesod.Auth.Util.PasswordStore as PS
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.UTF8     as BLU
import qualified Data.ByteString.Base64.Lazy   as B64
import qualified Encryption.Globals            as EG
import qualified Encryption.EncDec             as ED
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
        prompt "Available commands 'login' or 'register', 'q' to quit."
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
            empty <- BLU.toString . B64.encode <$> ED.encryptRandomCBC
              vaultKey
              EG.KS256
              (BLU.fromString "check[]")
            result <- register username loginHash empty
            case result of
              Left  err  -> putStrLn err >> loop state
              Right user -> do
                putStrLn
                  $  "New account created. here's some info about it:\n"
                  <> show user
                  <> "\nTo get started, log into your account."
                loop state
          "q" -> quit state
          _   -> putStrLn "Invalid command." >> loop state
      Just (token, vaultKey) -> do
        prompt
          "Available commands: 'deleteUser', 'getData', 'addEntries', 'removeEntries', 'logOut', 'q' to quit."
        command <- getLine
        case command of
          "deleteUser" -> do
            result <- deleteUser token
            case result of
              Left  err -> putStrLn err >> loop state
              Right _   -> putStrLn "Account deleted." >> loop emptyState
          "getData" -> case getStateData state of
            Nothing -> do
              result <- getData token vaultKey
              case result of
                Left err -> putStrLn err >> loop state
                Right entries ->
                  print (PPEntries entries) >> loop (state { getStateData = Just entries })
            Just entries -> print entries >> loop state
          "addEntries" -> do
            newEntries <- inputNewEntries
            result     <- case getStateData state of
              Nothing      -> putData token vaultKey newEntries
              Just entries -> putData' token vaultKey newEntries entries
            case result of
              Left  err -> putStrLn err >> loop state
              Right _   -> putStrLn "Entries successfully added."
                >> loop (state { getStateData = Nothing })
          "removeEntries" -> loop state
          "logOut"        -> loop emptyState
          "q"             -> quit state
          _               -> putStrLn "Invalid command." >> loop state

quit :: State -> IO State
quit state = putStrLn "Ok, bye!" >> pure state

prompt :: String -> IO ()
prompt msg = putStr (msg <> "\n>>>")

inputNewEntry :: IO Entry
inputNewEntry =
  Entry
    <$> (prompt "Input url:" >> getLine)
    <*> (prompt "Input username:" >> getLine)
    <*> (prompt "Input password (hidden):" >> getPassword)

inputNewEntries :: IO [Entry]
inputNewEntries = loop []
 where
  loop lst = do
    prompt "Commands: '+' to add new entry, 'e' to stop adding entries."
    command <- getLine
    case command of
      "+" -> inputNewEntry >>= (loop . flip (:) lst)
      "e" -> return lst
      _   -> loop lst

inputCredentials :: IO (String, String)
inputCredentials = do
  prompt "Input your mcferrin username:"
  username <- getLine
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
genLoginHash' master username = BLU.toString $ B64.encode $ B.fromStrict $ PS.pbkdf2
  (B.toStrict $ BLU.fromString master)
  (PS.makeSalt (B.toStrict $ zfill 8 (BLU.fromString username)))
  100101

genLoginHash :: B.ByteString -> String -> String
genLoginHash vaultKey username = BLU.toString $ B64.encode $ B.fromStrict $ PS.pbkdf2
  (B.toStrict vaultKey)
  (PS.makeSalt (B.toStrict $ zfill 8 (BLU.fromString username)))
  1


zfill :: Int -> B.ByteString -> B.ByteString
zfill n bs | B.length bs >= fromIntegral n = bs
           | otherwise = B.take (fromIntegral n) $ bs `B.append` B.replicate (fromIntegral n) 0

-- PS.pbkdf2 (BSU.fromString "HashMe") (PS.makeSalt (BSU.fromString "SALT8888")) 100000
