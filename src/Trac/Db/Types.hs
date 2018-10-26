{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Trac.Db.Types where

import Data.Functor.Identity
import Data.Text (Text)
import Data.Time.Clock

newtype TicketNumber = TicketNumber Integer
                     deriving (Show)

data TicketType = FeatureRequest | Bug | MergeReq | Task
                deriving (Show)

data Ticket = Ticket { ticketNumber :: TicketNumber
                     , ticketCreationTime :: UTCTime
                     , ticketFields :: Fields Identity
                     , ticketCreator :: Text
                     }
            deriving (Show)

data Priority = PrioLowest | PrioLow | PrioNormal | PrioHigh | PrioHighest
              deriving (Show)

data Status = New | Assigned | Patch | Merge | Closed | InfoNeeded | Upstream
            deriving (Show)

data Fields f = Fields { ticketType :: f TicketType
                       , ticketSummary :: f Text
                       , ticketComponent :: f Text
                       , ticketPriority :: f Priority
                       , ticketVersion :: f Text
                       , ticketMilestone :: f Text
                       , ticketDescription :: f Text
                       -- , ticketTypeOfFailure :: f Text
                       , ticketKeywords :: f [Text]
                       , ticketBlockedBy :: f [TicketNumber]
                       , ticketRelated :: f [TicketNumber]
                       , ticketBlocking :: f [TicketNumber]
                       , ticketDifferentials :: f [Differential]
                       , ticketTestCase :: f Text
                       , ticketStatus :: f Status
                       }

emptyFields :: Fields Maybe
emptyFields = Fields
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing

deriving instance Show (Fields Identity)
deriving instance Show (Fields Maybe)

newtype Differential = Differential Integer
                     deriving (Show)

data TicketChange = TicketChange { changeTime :: UTCTime
                                 , changeAuthor :: Text
                                 , changeFields :: Fields Maybe
                                 , changeComment :: Maybe Text
                                 }

                  deriving (Show)
