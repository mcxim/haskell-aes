{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# Language KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Client where

import           Data.Aeson                     ( object
                                                , withObject
                                                , ToJSON
                                                , FromJSON
                                                , (.:)
                                                , (.=)
                                                , encode
                                                , decode
                                                , parseJSON
                                                , toJSON
                                                )
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import           Servant.Client
import           Servant.API
import           Servant.API.ContentTypes       ( NoContent )
import qualified System.IO                     as SIO
import qualified Encryption.Utils              as EU
import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Lazy.UTF8     as BLU
import qualified Data.ByteString.Base64.Lazy   as B64
import qualified Encryption.EncDec             as ED
import qualified Encryption.Globals            as EG


run :: ClientM a -> IO (Either ClientError a)
run query = do
  manager' <- newManager defaultManagerSettings
  runClientM query (mkClientEnv manager' (BaseUrl Http "localhost" 5000 ""))

type Username = String
type Userdata = String -- b64 encoded

data User = User {_username :: Username, _userdata :: Userdata} deriving (Show, Generic)

instance ToJSON User where
  toJSON (User un ud) = object ["username" .= un, "data" .= ud]

data UserSchema = UserSchema {__username :: Username, __userdata :: Userdata, __id :: Int}
  deriving (Show, Generic)

instance FromJSON UserSchema where
  parseJSON = withObject "UserSchema" $ \obj -> do
    un  <- obj .: "username"
    ud  <- obj .: "data"
    id' <- obj .: "id"
    return (UserSchema un ud id')

newtype TokenInJSON = TokenInJSON {getTokenInJSON :: String} deriving (Show, Generic)

instance FromJSON TokenInJSON where
  parseJSON = withObject "TokenInJSON" $ \obj -> do
    t <- obj .: "token"
    return (TokenInJSON t)

data Credentials = Credentials String String String

instance FromJSON Credentials where
  parseJSON = withObject "Credentials" $ \obj -> do
    username <- obj .: "username"
    password <- obj .: "password"
    empty    <- obj .: "data"
    return (Credentials username password empty)

instance ToJSON Credentials where
  toJSON (Credentials username password empty) =
    object ["username" .= username, "password" .= password, "data" .= empty]

type Token = String

newtype DataInJSON = DataInJSON {getDataInJSON :: String}

instance FromJSON DataInJSON where
  parseJSON = withObject "DataInJSON" $ \obj -> do
    userdata <- obj .: "data"
    return (DataInJSON userdata)

instance ToJSON DataInJSON where
  toJSON (DataInJSON userdata) = object ["data" .= userdata]

type API =
       "register" :> ReqBody '[JSON] Credentials :> PostCreated '[JSON] UserSchema
  :<|> "login" :>  Header "Authorization" String :> Get '[JSON] TokenInJSON
  :<|> "user" :> (
                           Header "x-access-tokens" Token :>                               Get '[JSON] UserSchema
            :<|> "data" :> Header "x-access-tokens" Token :>                               Get '[JSON] DataInJSON
            :<|> "data" :> Header "x-access-tokens" Token :> ReqBody '[JSON] DataInJSON :> PutNoContent '[PlainText] NoContent -- '[PlainText] is just there to compile, no content returned at this endpoint.
            :<|>           Header "x-access-tokens" Token :>                               DeleteNoContent '[PlainText] NoContent
       ) 

api :: Proxy API
api = Proxy

registerHandler :: Credentials -> ClientM UserSchema
loginHandler :: Maybe Token -> ClientM TokenInJSON
getAllHandler :: Maybe Token -> ClientM UserSchema
getVaultHandler :: Maybe Token -> ClientM DataInJSON
putVaultHandler :: Maybe Token -> DataInJSON -> ClientM NoContent
deleteUserHandler :: Maybe Token -> ClientM NoContent
registerHandler :<|> loginHandler :<|> (getAllHandler :<|> getVaultHandler :<|> putVaultHandler :<|> deleteUserHandler)
  = client api

showError :: Either ClientError a -> Either String a
showError (Left  (FailureResponse _ response)) = Left $ show $ responseBody response
showError (Left  err                         ) = Left $ show err
showError (Right val                         ) = Right val

register :: String -> String -> String -> IO (Either String UserSchema)
register username loginHash empty =
  fmap showError . run . registerHandler $ Credentials username loginHash empty

login :: String -> String -> IO (Either String Token)
login username loginHash =
  showError
    .   fmap getTokenInJSON
    <$> (  run
        .  loginHandler
        .  Just
        .  ("Basic " <>)
        .  BLU.toString
        .  B64.encode
        .  BLU.fromString
        $  username
        <> ":"
        <> loginHash
        )

getAll :: Token -> IO (Either String UserSchema) -- works
getAll = fmap showError . run . getAllHandler . Just

getVault :: Token -> IO (Either String String)
getVault = fmap (showError . fmap getDataInJSON) . run . getVaultHandler . Just

putVault :: Token -> Userdata -> IO (Either String NoContent)
putVault token = fmap showError . run . putVaultHandler (Just token) . DataInJSON

deleteUser :: Token -> IO (Either String NoContent) -- works
deleteUser = fmap showError . run . deleteUserHandler . Just

data Entry = Entry {site :: String, username :: String, password :: String}
  deriving (Generic, Show)

instance ToJSON Entry

instance FromJSON Entry

-- newtype PPEntries = PPEntries [Entry]

pprintEntries :: [Entry] -> [Int] -> String
pprintEntries entries selects = header <> unlines (zipWith pprint [1 ..] entries) <> footer
 where
  header = "\n    Num  Website               Username         Password\n\n"
  footer = "\n "
  pprint :: Int -> Entry -> String
  pprint idx entry =
    "["
      <> (if idx `elem` selects then "*" else " ")
      <> "] "
      <> showIdx
      <> "."
      <> safeReplicate (4 - length showIdx) ' '
      <> showSite
      <> safeReplicate (22 - length showSite) ' '
      <> showUsername
      <> safeReplicate (17 - length showUsername) ' '
      <> showPassword
   where
    showIdx      = show idx
    showSite     = site entry
    showUsername = username entry
    showPassword = password entry
  safeReplicate :: Int -> Char -> String
  safeReplicate n a | n < 0     = []
                    | otherwise = replicate n a

-- instance Show PPEntries where
--   show (PPEntries entries) = header <> unlines (zipWith pprint [1 ..] entries) <> footer
--    where
--     header = "\nNum  Website               Username         Password\n\n"
--     footer = "\n "
--     pprint :: Int -> Entry -> String
--     pprint idx entry =
--       showIdx
--         <> "."
--         <> safeReplicate (4 - length showIdx) ' '
--         <> showSite
--         <> safeReplicate (22 - length showSite) ' '
--         <> showUsername
--         <> safeReplicate (17 - length showUsername) ' '
--         <> showPassword
--      where
--       showIdx      = show idx
--       showSite     = site entry
--       showUsername = username entry
--       showPassword = password entry
--     safeReplicate :: Int -> Char -> String
--     safeReplicate n a | n < 0     = []
--                       | otherwise = replicate n a

decryptData :: EG.Key -> String -> Either String [Entry]
decryptData key data' = do
  encrypted <- B64.decode (BLU.fromString data')
  let (check, rest) = B.splitAt 5 $ ED.decryptStream EG.CBC key EG.KS256 encrypted
  if check /= B.pack [99, 104, 101, 99, 107]
    then Left "Error: Decryption failed."
    else case decode rest of
      Nothing      -> Left "Error: JSON decoding error."
      Just entries -> Right entries

getData :: Token -> EG.Key -> IO (Either String [Entry])
getData token key = (>>= decryptData key) <$> getVault token

encryptData :: EG.Key -> [Entry] -> IO String
encryptData key entries = BLU.toString . B64.encode <$> ED.encryptRandomCBC
  key
  EG.KS256
  (B.append (B.pack [99, 104, 101, 99, 107]) . encode $ entries)

putData :: Token -> EG.Key -> [Entry] -> IO (Either String NoContent)
putData token key entriesToAdd = getData token key >>= \case
  Left  err     -> return $ Left err
  Right entries -> encryptData key (entries <> entriesToAdd) >>= putVault token

putData' :: Token -> EG.Key -> [Entry] -> IO (Either String NoContent)
putData' token key entries = encryptData key entries >>= putVault token


testB64 :: IO ()
testB64 = do
  fhandle  <- SIO.openFile "result.txt" SIO.ReadMode
  contents <- B.hGetContents fhandle
  EU.printBS contents
  print contents
  let encoded = B64.encode contents
  EU.printBS encoded
  print encoded
  let decoded = B64.decode encoded
  print decoded
  return ()
