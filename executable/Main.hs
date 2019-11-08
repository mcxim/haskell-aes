-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.


import           EncDec
import qualified System.IO                     as SIO
import qualified Data.ByteString               as B
import           Globals


main :: IO ()
main = test 'e' CBC "plaintext.txt" "result.txt"


test :: Char -> ModeOfOperation -> String -> String -> IO ()
test encdec mode file result = do
  fhandle  <- SIO.openFile file SIO.ReadMode
  khandle  <- SIO.openFile "key.txt" SIO.ReadMode
  ivhandle <- SIO.openFile "iv.txt" SIO.ReadMode
  contents <- B.hGetContents fhandle
  key      <- B.hGetContents khandle
  iv       <- B.hGetContents ivhandle
  B.writeFile result $ (if encdec == 'd' then decryptStream else encryptStream)
    mode
    iv
    key
    contents
  putStrLn "Done."

