{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Tickets where

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
-- getIssue
----------------------------------------------------------------------

type GetIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> Post '[JSON] IssueResp

getIssue :: AccessToken -> ProjectId -> IssueIid -> ClientM IssueResp
getIssue = client (Proxy :: Proxy GetIssueAPI) . Just


data IssueResp
    = IssueResp { irProjectId :: ProjectId
                , irIid :: IssueIid
                }
    deriving (Show)

instance FromJSON IssueResp where
    parseJSON = withObject "issue response" $ \o ->
      IssueResp <$> o .: "project_id"
                <*> o .: "iid"

----------------------------------------------------------------------
-- createIssue
----------------------------------------------------------------------

data CreateIssue
    = CreateIssue { ciIid :: Maybe IssueIid
                  , ciTitle :: Text
                  , ciLabels :: Maybe Labels
                  , ciCreatedAt :: Maybe UTCTime
                  , ciDescription :: Maybe Text
                  , ciMilestoneId :: Maybe (Maybe MilestoneId)
                  , ciWeight :: Maybe Weight
                  }

instance ToJSON CreateIssue where
    toJSON CreateIssue{..} = object
        [ "iid" .= ciIid
        , "title" .= ciTitle
        , "labels" .= ciLabels
        , "created_at" .= ciCreatedAt
        , "description" .= ciDescription
        , "milestone_id" .= ciMilestoneId
        , "weight" .= ciWeight
        ]

type CreateIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> ReqBody '[JSON] CreateIssue
    :> Post '[JSON] IssueResp

createIssue :: AccessToken -> ProjectId -> CreateIssue -> ClientM IssueResp
createIssue = client (Proxy :: Proxy CreateIssueAPI) . Just

----------------------------------------------------------------------
-- editIssue
----------------------------------------------------------------------

data EditIssue
    = EditIssue { eiTitle       :: Maybe Text
                , eiDescription :: Maybe Text
                , eiMilestoneId :: Maybe (Maybe MilestoneId)
                , eiLabels      :: Maybe Labels
                , eiStatus      :: Maybe StatusEvent
                , eiUpdateTime  :: Maybe UTCTime
                , eiWeight      :: Maybe Weight
                }
    deriving (Show)

instance ToJSON EditIssue where
    toJSON EditIssue{..} = object
        $ catMaybes
        [ "title" .=? eiTitle
        , "description" .=? eiDescription
        , "milestone_id" .=? eiMilestoneId
        , "labels" .=? eiLabels
        , "state_event" .=? eiStatus
        , "updated_at" .=? eiUpdateTime
        , "weight" .=? eiWeight
        ]

type EditIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> ReqBody '[JSON] EditIssue
    :> Put '[JSON] IssueResp

nullEditIssue :: EditIssue -> Bool
nullEditIssue (EditIssue a b c d e _ g) =
    isNothing a && isNothing b && isNothing c &&
    isNothing d && isNothing e && isNothing g
    -- N.B. Ignore update time

editIssue :: AccessToken -> ProjectId -> IssueIid -> EditIssue -> ClientM IssueResp
editIssue = client (Proxy :: Proxy EditIssueAPI) . Just

----------------------------------------------------------------------
-- createIssueNote
----------------------------------------------------------------------

data CreateIssueNote
    = CreateIssueNote { cinBody :: Text
                      , cinCreatedAt :: Maybe UTCTime
                      }

instance ToJSON CreateIssueNote where
    toJSON CreateIssueNote{..} = object
        [ "body" .= cinBody
        , "created_at" .= cinCreatedAt
        ]

data IssueNoteResp
    = IssueNoteResp { inrId :: Int
                    }
    deriving (Show)

instance FromJSON IssueNoteResp where
    parseJSON = withObject "issue note response" $ \o ->
      IssueNoteResp <$> o .: "id"

type CreateIssueNoteAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid :> "notes"
    :> ReqBody '[JSON] CreateIssueNote
    :> Post '[JSON] IssueNoteResp

createIssueNote :: AccessToken -> ProjectId -> IssueIid -> CreateIssueNote -> ClientM IssueNoteResp
createIssueNote = client (Proxy :: Proxy CreateIssueNoteAPI) . Just

----------------------------------------------------------------------
-- deleteIssue
----------------------------------------------------------------------

type DeleteIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> Delete '[] NoContent

deleteIssue :: AccessToken -> ProjectId -> IssueIid -> ClientM ()
deleteIssue tok prj iid = void $ client (Proxy :: Proxy DeleteIssueAPI) (Just tok) prj iid

----------------------------------------------------------------------
-- createMilestone
----------------------------------------------------------------------

type CreateMilestoneAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "milestones"
    :> ReqBody '[JSON] CreateMilestone
    :> Post '[JSON] CreateMilestoneResp

data CreateMilestone
    = CreateMilestone { cmTitle :: Text
                      , cmDescription :: Text
                      , cmDueDate :: Maybe UTCTime
                      , cmStartDate :: Maybe UTCTime
                      }

instance ToJSON CreateMilestone where
    toJSON CreateMilestone{..} = object
        [ "title" .= cmTitle
        , "description" .= cmDescription
        , "due_date" .= cmDueDate
        , "start_date" .= cmStartDate
        ]

data CreateMilestoneResp = CreateMilestoneResp MilestoneId

instance FromJSON CreateMilestoneResp where
    parseJSON = withObject "create milestone response" $ \o -> do
        CreateMilestoneResp <$> o .: "id"

createMilestone :: AccessToken -> ProjectId -> CreateMilestone -> ClientM MilestoneId
createMilestone tok prj cm = do
    CreateMilestoneResp mid <- client (Proxy :: Proxy CreateMilestoneAPI) (Just tok) prj cm
    return mid

----------------------------------------------------------------------
-- listMilestones
----------------------------------------------------------------------

type ListMilestonesAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "milestones"
    :> Get '[JSON] [Milestone]

data Milestone = Milestone Text MilestoneId

instance FromJSON Milestone where
    parseJSON = withObject "milestone" $ \o -> do
        Milestone <$> o .: "title" <*> o .: "id"

listMilestones :: AccessToken -> ProjectId -> ClientM [Milestone]
listMilestones tok prj = do
    client (Proxy :: Proxy ListMilestonesAPI) (Just tok) prj
