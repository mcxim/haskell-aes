module Encryption.PBKDF2 where

import qualified Data.ByteString               as B
import           Encryption.Utils

-- fFunc :: String -> String -> Int -> Int -> B.ByteString
-- fFunc password salt c i = foldl1 bsXor us
--  where
--   uGenerator = undefined

pbkdf2 prFunction derivedKeyLength salt iterations passPhrase = undefined
