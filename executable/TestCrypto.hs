module TestCrypto where

import           Control.Exception              ( evaluate )
import qualified Data.ByteString.Lazy          as B
import           Encryption.EncDec
import           Encryption.Globals
import           Encryption.Utils
import qualified System.IO                     as SIO

-- main :: IO ()
-- main = test "e1" ECB "plaintext.txt" "result.txt"
st :: String -> IO ()
st "e" = test "e" "plaintext.txt" "result.txt"
st "d" = test "d" "result.txt" "plaintext.txt"
st _              = st "e"

-- test' :: String -> ModeOfOperation -> String -> String -> IO ()
-- test' [encdec, keySize'] mode file result = do
--   fhandle  <- SIO.openFile file SIO.ReadMode
--   khandle  <- SIO.openFile "key.txt" SIO.ReadMode
--   ivhandle <- SIO.openFile "iv.txt" SIO.ReadMode
--   contents <- B.hGetContents fhandle
--   key      <- B.hGetContents khandle
--   iv       <- B.hGetContents ivhandle
--   let info = (if encdec == 'd' then decryptStream else encryptStream)
--         mode
--         iv
--         key
--         keySize
--         contents
--   -- putStrLn $ "plaintext: " ++ reprBS contents
--   -- putStrLn $ "key: " ++ reprBS key
--   -- putStrLn $ "iv: " ++ reprBS iv
--   -- putStrLn $ "Mode of operation: " ++ show mode
--   -- putStrLn $ "result: " ++ reprBS info
--   B.writeFile result info
--  where
--   keySize = case keySize' of
--     '1' -> KS128
--     '2' -> KS192
--     '3' -> KS256
--     _   -> KS128
-- test' _ _ _ _ = error "Usage: TODO"


test :: String -> String -> String -> IO ()
test (encdec : _) file result = do
  fhandle  <- SIO.openFile file SIO.ReadMode
  khandle  <- SIO.openFile "key.txt" SIO.ReadMode
  contents <- B.hGetContents fhandle
  key      <- B.hGetContents khandle
  info     <- if encdec == 'd'
    then evaluate (decryptStream CBC key KS256 contents)
    else encryptRandomCBC key KS256 contents
  putStrLn $ "plaintext: " ++ reprBS contents
  putStrLn $ "key: " ++ reprBS key
  putStrLn $ "result: " ++ reprBS info
  B.writeFile result info
test _ _ _ = error "Usage: TODO"
