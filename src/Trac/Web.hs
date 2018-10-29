{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Trac.Web where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import Data.Semigroup
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Proxy
import Data.Time.Clock
import Servant.API
import Servant.Client
import Trac.Db.Types

----------------------------------------------------------------------
-- fetchTicketAttachment
----------------------------------------------------------------------

deriving instance ToHttpApiData TicketNumber

type FetchTicketAttachmentAPI =
    "raw-attachment" :> "ticket"
    :> Capture "ticket" TicketNumber
    :> Capture "filename" Text
    :> Get '[OctetStream] ByteString

fetchTicketAttachment :: TicketNumber -> Text -> ClientM ByteString
fetchTicketAttachment = client (Proxy :: Proxy FetchTicketAttachmentAPI)
