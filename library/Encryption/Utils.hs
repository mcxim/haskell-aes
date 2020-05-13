module Encryption.Utils where

import qualified Data.ByteString.Lazy          as B
import qualified Data.Bits
import           Numeric                        ( showHex )
import           Data.Char                      ( toUpper )


-- Split bytestring every n bytes.
splitEvery :: Int -> B.ByteString -> [B.ByteString]
splitEvery n list | list == B.empty = []
                  | otherwise       = first : splitEvery n rest
  where (first, rest) = B.splitAt (fromIntegral n) list

-- Rotate word left n times.
rotWordLeft :: Int -> B.ByteString -> B.ByteString
rotWordLeft n word =
  B.drop (fromIntegral n) word `B.append` B.take (fromIntegral n) word

-- Rotate word right n times.
rotWordRight :: Int -> B.ByteString -> B.ByteString
rotWordRight n word =
  B.drop (len - fromIntegral n) word
    `B.append` B.take (len - fromIntegral n) word
  where len = B.length word

-- ByteString xor.
bsXor :: B.ByteString -> B.ByteString -> B.ByteString
bsXor xs ys = B.pack $ B.zipWith Data.Bits.xor xs ys

-- Represent a bytestring in readable format.
reprBS :: B.ByteString -> String
reprBS =
  unwords . map (("0x" ++) . zfill 2 . map toUpper . (`showHex` "")) . B.unpack

-- Print a bytestring in readable format.
printBS :: B.ByteString -> IO ()
printBS = putStrLn . reprBS

-- Represent a list of bytestrings in readable format.
reprBSL :: [B.ByteString] -> String
reprBSL = unlines . map reprBS

-- Print a list of bytestrings in readable format.
printBSL :: [B.ByteString] -> IO ()
printBSL = putStrLn . reprBSL

-- Fill the string representing the numbers with zeros so its length is num.
zfill :: Int -> String -> String
zfill num str = replicate (num - length str) '0' ++ str
