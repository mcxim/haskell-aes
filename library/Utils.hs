module Utils where

import qualified Data.ByteString               as B

snoc :: [a] -> a -> [a]
snoc lst x = lst ++ [x]

splitEvery :: Int -> B.ByteString -> [B.ByteString]
splitEvery n list | list == B.empty = []
                  | otherwise       = first : splitEvery n rest
  where (first, rest) = B.splitAt n list

rotWordLeft :: Int -> B.ByteString -> B.ByteString
rotWordLeft n word = B.drop n word `B.append` B.take n word
