{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Client where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import           Servant.Client
import           Servant.API
import           Servant.Types.SourceT          ( foreach )


type Username = String
type Userdata = String -- b64 encoded

data User = User {username :: Username, userdata :: Userdata} deriving (Show, Generic)

instance ToJSON User where
  toJSON (User un ud) = object ["username" .= un, "data" .= ud]


data UserWId = UserWId {username' :: Username, userdata' :: Userdata, uid :: Int}
  deriving (Show, Generic)

instance FromJSON UserWId where
  parseJSON = withObject "UserWId" $ \obj -> do
    un  <- obj .: "username"
    ud  <- obj .: "data"
    id' <- obj .: "id"
    return UserWId { username' = un, userdata' = ud, uid = id' }


type API = "user" :> (
            Get '[JSON] [UserWId]
      :<|>  Capture "userid" Int :> Get '[JSON] UserWId
      :<|>  ReqBody '[JSON] User :> Post '[JSON] UserWId
      :<|>  ReqBody '[JSON] User :> PutAccepted '[JSON] UserWId
      :<|>  Capture "userid" Int :> DeleteAccepted '[JSON] UserWId
      )

api :: Proxy API
api = Proxy

getUserById :: Int -> ClientM UserWId
getUsers :: ClientM [UserWId]
postUser :: User -> ClientM UserWId
putUser :: User -> ClientM UserWId
deleteUser :: Int -> ClientM UserWId
getUsers :<|> getUserById :<|> postUser :<|> putUser :<|> deleteUser =
  client api

post = postUser (User "find me" "somebodytolove")
getall = getUsers
get = getUserById 3


run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM get
                    (mkClientEnv manager' (BaseUrl Http "localhost" 5000 ""))
  case res of
    Left  err  -> putStrLn $ "Error: " ++ show err
    Right user -> print user



-- test :: IO ()
-- test = do
--   fhandle  <- SIO.openFile "key.txt" SIO.ReadMode
--   contents <- B.hGetContents fhandle
--   EU.printBS contents
--   print contents
--   let encoded = B64.encode contents
--   EU.printBS encoded
--   print encoded
--   let decoded = B64.decode encoded
--   print decoded
--   return ()
