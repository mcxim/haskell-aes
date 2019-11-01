-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import           EncDec
import qualified System.IO                     as SIO
import qualified Data.ByteString               as B
import           Utils
import           Globals

main :: IO ()
main = do
  fhandle  <- SIO.openFile "plaintext.txt" SIO.ReadMode
  khandle  <- SIO.openFile "key.txt" SIO.ReadMode
  ivhandle <- SIO.openFile "iv.txt" SIO.ReadMode
  contents <- B.hGetContents fhandle
  key      <- B.hGetContents khandle
  iv       <- B.hGetContents ivhandle
  putStrLn "contents: "
  printBS $ pad contents
  putStrLn "key: "
  printBS $ padKeyIV key
  putStrLn "iv: "
  printBS $ padKeyIV iv
  putStrLn "result: "
  printBS $ decryptStream CBC iv key contents
  B.writeFile "plaintext.txt" $ decryptStream ECB iv key contents
  putStrLn "Done."
