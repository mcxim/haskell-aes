-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.


import           EncDec
import qualified System.IO                     as SIO
import qualified Data.ByteString               as B
import           Globals
import           Utils


main :: IO ()
main = test "e1" ECB "plaintext.txt" "result.txt"

st :: String -> IO ()
st ('e' : params) = test ('e' : params) CBC "plaintext.txt" "result.txt"
st ('d' : params) = test ('d' : params) CBC "result.txt" "plaintext.txt"
st _ = st "e1"

test :: String -> ModeOfOperation -> String -> String -> IO ()
test [encdec, keySize'] mode file result = do
  fhandle  <- SIO.openFile file SIO.ReadMode
  khandle  <- SIO.openFile "key.txt" SIO.ReadMode
  ivhandle <- SIO.openFile "iv.txt" SIO.ReadMode
  contents <- B.hGetContents fhandle
  key      <- B.hGetContents khandle
  iv       <- B.hGetContents ivhandle
  let info = (if encdec == 'd' then decryptStream else encryptStream)
        mode
        iv
        key
        keySize
        contents
  -- putStrLn $ "plaintext: " ++ reprBS contents
  -- putStrLn $ "key: " ++ reprBS key
  -- putStrLn $ "iv: " ++ reprBS iv
  -- putStrLn $ "Mode of operation: " ++ show mode
  -- putStrLn $ "result: " ++ reprBS info
  B.writeFile result info
 where
  keySize = case keySize' of
    '1' -> KS128
    '2' -> KS192
    '3' -> KS256
    _   -> KS128
test _ _ _ _ = error "Usage: TODO"
