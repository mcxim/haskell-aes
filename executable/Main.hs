{-# LANGUAGE LambdaCase #-}

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
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Text.Read                      ( readMaybe )
import           Debug.Trace                    ( trace )

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
  "Welcome to the McFerrin password manager! This is a CLI tool that lets you communicate "
    <> "with the server."

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
          $  "Available commands: 'deleteUser', 'getData', 'addEntries', "
          <> "'removeEntries', 'logOut', 'q' to quit."
        command <- getLine
        case command of
          "deleteUser" -> do
            result <- deleteUser token
            case result of
              Left  err -> putStrLn err >> loop state
              Right _   -> putStrLn "Account deleted." >> loop emptyState
          "getData" ->
            (case getStateData state of
                Nothing      -> getData token vaultKey
                Just entries -> return $ Right entries
              )
              >>= \case
                    Left  err     -> putStrLn err >> loop state
                    Right entries -> putStrLn (pprintEntries entries []) >> loop state
          "addEntries" -> do
            newEntries <- inputNewEntries
            result     <- case getStateData state of
              Nothing      -> putData token vaultKey newEntries
              Just entries -> putData' token vaultKey (entries <> newEntries)
            case result of
              Left  err -> putStrLn err >> loop state
              Right _   -> putStrLn "Entries successfully added."
                >> loop (state { getStateData = Nothing })
          "removeEntries" ->
            (case getStateData state of
                Nothing      -> getData token vaultKey
                Just entries -> return $ Right entries
              )
              >>= \case
                    Left  err     -> putStrLn err >> loop state
                    Right entries -> do
                      print entries
                      nums <- inputEntriesToRemove entries
                      let idxs      = map pred nums
                      let remaining = removeIdxsFromList idxs entries
                      result <- putData' token vaultKey remaining
                      case result of
                        Left err -> putStrLn err >> loop state
                        Right _ ->
                          putStrLn (show (length idxs) <> " entries successfully removed.")
                            >> loop (state { getStateData = Nothing })
          "logOut" -> loop emptyState
          "q"      -> quit state
          _        -> putStrLn "Invalid command." >> loop state


quit :: State -> IO State
quit state = putStrLn "Ok, bye!" >> pure state

prompt :: String -> IO ()
prompt msg = putStr (msg <> "\n>>>")

removeIdxsFromList :: [Int] -> [a] -> [a]
removeIdxsFromList idxs lst = map (fromMaybe undefined) $ filter isJust $ helper
  (filter (\i -> i < length idxs || i >= 0) idxs)
  (map Just lst)
 where
  helper :: [Int] -> [Maybe a] -> [Maybe a]
  helper []           l = l
  helper (idx : idxs) l = helper idxs $ lft ++ [Nothing] ++ rgt
    where (lft, _ : rgt) = splitAt idx l

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

inputEntriesToRemove :: [Entry] -> IO [Int]
inputEntriesToRemove entries = loop []
 where
  loop :: [Int] -> IO [Int]
  loop lst = do
    putStrLn $ pprintEntries entries lst
    prompt
      "To select/deselect an entry enter its number. To delete selected entries enter 'd'."
    input <- getLine
    if input == "d"
      then return lst
      else case readMaybe input :: Maybe Int of
        Just n  -> loop (if n `elem` lst then removeItem n lst else n : lst)
        Nothing -> loop lst

removeItem _ [] = []
removeItem x (y : ys) | x == y    = removeItem x ys
                      | otherwise = y : removeItem x ys


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
