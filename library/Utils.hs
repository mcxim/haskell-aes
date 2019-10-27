module Utils where

import qualified Data.ByteString               as B
import qualified Data.Bits
import           Numeric                        ( showHex )

snoc :: [a] -> a -> [a]
snoc lst x = lst ++ [x]

splitEvery :: Int -> B.ByteString -> [B.ByteString]
splitEvery n list | list == B.empty = []
                  | otherwise       = first : splitEvery n rest
  where (first, rest) = B.splitAt n list

rotWordLeft :: Int -> B.ByteString -> B.ByteString
rotWordLeft n word = B.drop n word `B.append` B.take n word

bsXor :: B.ByteString -> B.ByteString -> B.ByteString
bsXor xs ys = B.pack $ B.zipWith Data.Bits.xor xs ys

printBS :: B.ByteString -> IO ()
printBS = print . B.unpack

printBSL :: [B.ByteString] -> IO ()
printBSL = putStrLn . unlines . map (unwords . map (`showHex` "") . B.unpack)
