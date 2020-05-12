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


-- Run a client operation and return IO of the result.
run :: ClientM a -> IO (Either ClientError a)
run query = do
  manager' <- newManager defaultManagerSettings
  runClientM query (mkClientEnv manager' (BaseUrl Http "localhost" 5000 ""))

-- Declare type aliases for username and encrypted data.
type Username = String
type Userdata = String -- b64 encoded

-- Declare algebraic data type representing a user,
-- we will recieve this type in json format from the server.
data UserSchema = UserSchema {__username :: Username, __userdata :: Userdata, __id :: Int}
  deriving (Show, Generic)

-- Instruct Aeson on how to convert from JSON to UserSchema.
instance FromJSON UserSchema where
  parseJSON = withObject "UserSchema" $ \obj -> do
    un  <- obj .: "username"
    ud  <- obj .: "data"
    id' <- obj .: "id"
    return (UserSchema un ud id')

-- Declare wrapper for token inside JSON so that it can be recieved from the server.
newtype TokenInJSON = TokenInJSON {getTokenInJSON :: String} deriving (Show, Generic)

-- Instruct Aeson on how to convert from JSON to the token wrapper.
instance FromJSON TokenInJSON where
  parseJSON = withObject "TokenInJSON" $ \obj -> do
    t <- obj .: "token"
    return (TokenInJSON t)

-- Declare algebraic data type representing the credentials of a user.
data Credentials = Credentials String String String

-- Instruct Aeson on how to convert from Credentials to JSON.
instance ToJSON Credentials where
  toJSON (Credentials username password empty) =
    object ["username" .= username, "password" .= password, "data" .= empty]

-- Declare type alias for a token, which is a string.
type Token = String

-- Declare a wrapper for data inside JSON so that it can be recieved from the server.
newtype DataInJSON = DataInJSON {getDataInJSON :: String}

-- Instruct Aeson on how to convert from JSON to DataInJSON
instance FromJSON DataInJSON where
  parseJSON = withObject "DataInJSON" $ \obj -> do
    userdata <- obj .: "data"
    return (DataInJSON userdata)

-- Instruct Aeson on how to convert from DataInJSON to JSON
instance ToJSON DataInJSON where
  toJSON (DataInJSON userdata) = object ["data" .= userdata]

-- The type level definition of the api. From this servant understands how it should query it.
-- it is also comprehensive and a very concise description of the McFerrin API.
type API =
       "register" :> ReqBody '[JSON] Credentials :> PostCreated '[JSON] UserSchema
  :<|> "login" :>  Header "Authorization" String :> Get '[JSON] TokenInJSON
  :<|> "user" :> (
                           Header "x-access-tokens" Token :>                               Get '[JSON] UserSchema
            :<|> "data" :> Header "x-access-tokens" Token :>                               Get '[JSON] DataInJSON
            :<|> "data" :> Header "x-access-tokens" Token :> ReqBody '[JSON] DataInJSON :> PutNoContent '[PlainText] NoContent -- '[PlainText] is just there to compile, no content returned at this endpoint.
            :<|>           Header "x-access-tokens" Token :>                               DeleteNoContent '[PlainText] NoContent
       ) 

-- Boilerplate
api :: Proxy API
api = Proxy

-- Naming the functions that query the API.
registerHandler :: Credentials -> ClientM UserSchema
loginHandler :: Maybe Token -> ClientM TokenInJSON
getAllHandler :: Maybe Token -> ClientM UserSchema
getVaultHandler :: Maybe Token -> ClientM DataInJSON
putVaultHandler :: Maybe Token -> DataInJSON -> ClientM NoContent
deleteUserHandler :: Maybe Token -> ClientM NoContent
registerHandler :<|> loginHandler :<|> (getAllHandler :<|> getVaultHandler :<|> putVaultHandler :<|> deleteUserHandler)
  = client api

-- Function to show ClientErrors better. This function is penetrated into the query functions
-- so that they in case of an error they return a readable string instead of ClientError.
showError :: Either ClientError a -> Either String a
showError (Left  (FailureResponse _ response)) = Left $ show $ responseBody response
showError (Left  err                         ) = Left $ show err
showError (Right val                         ) = Right val

-- Register a new user/account.
register :: String -> String -> String -> IO (Either String UserSchema)
register username loginHash empty =
  fmap showError . run . registerHandler $ Credentials username loginHash empty

-- Login into the system and recieve a jwt access token.
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

-- Get all available information about the user.
getAll :: Token -> IO (Either String UserSchema)
getAll = fmap showError . run . getAllHandler . Just

-- Get the user's encrypted vault.
getVault :: Token -> IO (Either String String)
getVault = fmap (showError . fmap getDataInJSON) . run . getVaultHandler . Just

-- Change the contents of the user's vault.
putVault :: Token -> Userdata -> IO (Either String NoContent)
putVault token = fmap showError . run . putVaultHandler (Just token) . DataInJSON

-- Delete the account of the user.
deleteUser :: Token -> IO (Either String NoContent) -- works
deleteUser = fmap showError . run . deleteUserHandler . Just

-- Declare algebraic data type representing an entry in a vault.
data Entry = Entry {site :: String, username :: String, password :: String}
  deriving (Generic, Show)

-- Automatically generate instances for converting this type from and to JSON.
instance ToJSON Entry
instance FromJSON Entry

-- Represent the list of entries constituting the vault in a human readable format.
-- Also supports selecting items from the vault.
pprintEntries :: [Entry] -> [Int] -> String
pprintEntries entries selects = header <> unlines (zipWith pprint [1 ..] entries) <> footer
 where
  header
    = "\n    Num | Website              | Username        | Password\n------------------------------------------------------------\n"
  footer = "\n "
  pprint :: Int -> Entry -> String
  pprint idx entry =
    "["
      <> (if idx `elem` selects then "X" else " ")
      <> "] "
      <> showIdx
      <> "."
      <> safeReplicate (3 - length showIdx) ' '
      <> "| "
      <> showSite
      <> safeReplicate (21 - length showSite) ' '
      <> "| "
      <> showUsername
      <> safeReplicate (16 - length showUsername) ' '
      <> "| "
      <> showPassword
   where
    showIdx      = show idx
    showSite     = site entry
    showUsername = username entry
    showPassword = password entry
  safeReplicate :: Int -> Char -> String
  safeReplicate n a | n < 0     = []
                    | otherwise = replicate n a

decryptData key data' = do
  encrypted <- B64.decode (BLU.fromString data')
  let (check, rest) = B.splitAt 5 $ ED.decryptStream EG.CBC key EG.KS256 encrypted
  if check /= B.pack [99, 104, 101, 99, 107]
    then Left "Error: Decryption failed."
    else case decode rest of
      Nothing      -> Left "Error: JSON decoding error."
      Just entries -> Right entries

-- Combine previously defined functions to a function that gets the vault of the user given a
-- JWT token.
getData :: Token -> EG.Key -> IO (Either String [Entry])
getData token key = (>>= decryptData key) <$> getVault token

-- Encrypt the vault using given key.
encryptData :: EG.Key -> [Entry] -> IO String
encryptData key entries = BLU.toString . B64.encode <$> ED.encryptRandomCBC
  key
  EG.KS256
  (B.append (B.pack [99, 104, 101, 99, 107]) . encode $ entries)

-- Download the data of the user, add the new entries to it and send it back.
putData :: Token -> EG.Key -> [Entry] -> IO (Either String NoContent)
putData token key entriesToAdd = getData token key >>= \case
  Left  err     -> return $ Left err
  Right entries -> encryptData key (entries <> entriesToAdd) >>= putVault token

-- Variant of putData but for replacing the data without downloading old data.
putData' :: Token -> EG.Key -> [Entry] -> IO (Either String NoContent)
putData' token key entries = encryptData key entries >>= putVault token