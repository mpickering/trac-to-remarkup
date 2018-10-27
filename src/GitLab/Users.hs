{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Users where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import Data.Semigroup
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Proxy
import Data.String
import Data.Time.Clock
import Servant.API
import Servant.Client
import GitLab.Common

----------------------------------------------------------------------
-- createUser
----------------------------------------------------------------------

data CreateUser
    = CreateUser { cuEmail       :: Text
                 , cuUsername    :: Text
                 , cuName        :: Text
                 , cuSkipConfirmation :: Bool
                 }
    deriving (Show)

instance ToJSON CreateUser where
    toJSON CreateUser{..} = object
        [ "email" .= cuEmail
        , "username" .=  cuUsername
        , "name" .=  cuName
        , "skip_confirmation" .= cuSkipConfirmation
        ]

data CreateUserResp = CreateUserResp UserId

instance FromJSON CreateUserResp where
    parseJSON = withObject "create user response" $ \o -> do
        CreateUserResp <$> o .: "id"

type CreateUserAPI =
    GitLabRoot :> "users"
    :> ReqBody '[JSON] CreateUser
    :> Put '[JSON] CreateUserResp

createUser :: AccessToken -> CreateUser -> ClientM CreateUserResp
createUser = client (Proxy :: Proxy CreateUserAPI) . Just


----------------------------------------------------------------------
-- findUserByUsername
----------------------------------------------------------------------

type FindUserByUsernameAPI =
    GitLabRoot :> "users"
    :> QueryParam "username" Text
    :> Get '[JSON] [CreateUserResp]

findUserByUsername :: AccessToken -> Text -> ClientM (Maybe UserId)
findUserByUsername tok username = do
    res <- client (Proxy :: Proxy FindUserByUsernameAPI) (Just tok) (Just username)
    return $ case res of
               [] -> Nothing
               [CreateUserResp uid] -> Just uid
               _ -> error $ "Multiple users with id "<>show username
