{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Tickets where

import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import Data.Semigroup
import Data.Aeson
import Data.Proxy
import Data.String
import Data.Time.Clock
import Servant.API
import Servant.Client

newtype ProjectId = ProjectId Int
                  deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

newtype Labels = Labels (S.Set Text)
               deriving (Semigroup, Monoid, Show)

instance IsString Labels where
    fromString = Labels . S.singleton . T.pack

instance ToJSON Labels where
    toJSON (Labels lbls) = toJSON $ T.intercalate "," (S.toList lbls)

newtype IssueIid = IssueIid Int
                 deriving (Eq, Ord, Show, ToJSON, FromJSON, ToHttpApiData)

data IssueResp
    = IssueResp { irProjectId :: ProjectId
                , irIid :: IssueIid
                }

instance FromJSON IssueResp where
    parseJSON = withObject "issue response" $ \o ->
      IssueResp <$> o .: "project_id"
                <*> o .: "iid"

data CreateIssue
    = CreateIssue { ciIid :: Maybe IssueIid
                  , ciTitle :: Text
                  , ciLabels :: Maybe Labels
                  , ciCreatedAt :: Maybe UTCTime
                  , ciDescription :: Maybe Text
                  }

instance ToJSON CreateIssue where
    toJSON CreateIssue{..} = object
        [ "iid" .= ciIid
        , "title" .= ciTitle
        , "labels" .= ciLabels
        , "created_at" .= ciCreatedAt
        , "description" .= ciDescription
        ]

type CreateIssueAPI = "projects" :> Capture "id" ProjectId :> "issues" :> ReqBody '[JSON] CreateIssue :> Post '[JSON] IssueResp

createIssue :: ProjectId -> CreateIssue -> ClientM IssueResp
createIssue = client (Proxy :: Proxy CreateIssueAPI)

data CreateIssueNote
    = CreateIssueNote { cinBody :: Text
                      , cinCreatedAt :: Maybe UTCTime
                      }

instance ToJSON CreateIssueNote where
    toJSON CreateIssueNote{..} = object
        [ "body" .= cinBody
        , "created_at" .= cinCreatedAt
        ]

type CreateIssueNoteAPI = "projects" :> Capture "id" ProjectId :> "issues" :> Capture "iid" IssueIid :> "notes" :> ReqBody '[JSON] CreateIssueNote :> Post '[JSON] IssueResp

createIssueNote :: ProjectId -> IssueIid -> CreateIssueNote -> ClientM IssueResp
createIssueNote = client (Proxy :: Proxy CreateIssueNoteAPI)
